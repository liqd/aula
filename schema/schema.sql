-- for every haskell data type, there is a table of the same name.
-- fields are named after record selectors.  this rule is broken where
-- the SQL schema diverges from the Haskell types, and where we
-- encountered name clashes with SQL keywords.
--
-- likes and votes do not need serial numbers.


----------------------------------------------------------------------
-- idea

CREATE TABLE IF NOT EXISTS idea (
    id          bigserial   PRIMARY KEY,
    created_by  bigint      NOT NULL REFERENCES users (id),
    created_at  timestamptz NOT NULL DEFAULT now(),
    changed_by  bigint      NOT NULL REFERENCES users (id),
    changed_at  timestamptz NOT NULL DEFAULT now()
    title       text        NOT NULL,
    description text        NOT NULL,
    category    category    NOT NULL,
    idea_space  bigint      REFERENCES idea_space (id),  -- 'NULL' == 'SchoolSpace'
    topic       bigint      REFERENCES topic (id),
    feasible    bigint      REFERENCES feasible (id),
);

CREATE TYPE category AS ENUM
    ('rule', 'equipment', 'class', 'time', 'environment');

CREATE TABLE IF NOT EXISTS idea_like (
    idea        bigint      NOT NULL REFERENCES idea (id),
    created_by  bigint      NOT NULL REFERENCES users (id),
    created_at  timestamptz NOT NULL DEFAULT now(),
    changed_by  bigint      NOT NULL REFERENCES users (id),
    changed_at  timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS idea_vote (
    idea       bigint          NOT NULL REFERENCES idea (id),
    created_by bigint          NOT NULL REFERENCES users (id),
    created_at timestamptz     NOT NULL DEFAULT now(),
    changed_by bigint          NOT NULL REFERENCES users (id),
    changed_at timestamptz     NOT NULL DEFAULT now(),
    val        idea_vote_value NOT NULL
);

CREATE TYPE idea_vote_value AS ENUM
    ('yes', 'no', 'neutral');

CREATE TABLE IF NOT EXISTS feasible (
    id         bigserial   PRIMARY KEY,
    created_by bigint      NOT NULL REFERENCES users (id),
    created_at timestamptz NOT NULL DEFAULT now(),
    changed_by bigint      NOT NULL REFERENCES users (id),
    changed_at timestamptz NOT NULL DEFAULT now(),
    val        bool        NOT NULL,
    reason     text
    -- FIXME: CONSTRAINT: when (not val) (reason not null)
);


----------------------------------------------------------------------
-- comment

CREATE TABLE IF NOT EXISTS comment (
    id             bigserial   PRIMARY KEY,
    created_by     bigint      NOT NULL REFERENCES users (id),
    created_at     timestamptz NOT NULL DEFAULT now(),
    changed_by     bigint      NOT NULL REFERENCES users (id),
    changed_at     timestamptz NOT NULL DEFAULT now(),
    text           text        NOT NULL,
    parent_comment bigint      REFERENCES comment (id),
    parent_idea    bigint      REFERENCES idea (id)
    CHECK (parent_comment IS NULL) <> (parent_idea IS NULL)
        -- FIXME: move the comment tree to a separate association
        -- table?  (we will often be looking for children rather than
        -- parents.)
);

CREATE TABLE IF NOT EXISTS comment_vote (
    comment    bigint      NOT NULL REFERENCES comment (id),
    created_by bigint      NOT NULL REFERENCES users (id),
    created_at timestamptz NOT NULL DEFAULT now(),
    changed_by bigint      NOT NULL REFERENCES users (id),
    changed_at timestamptz NOT NULL DEFAULT now(),
    val        up_down     NOT NULL
);

CREATE TYPE up_down AS ENUM
    ('up', 'down');


----------------------------------------------------------------------
-- idea space

CREATE TABLE IF NOT EXISTS school_class (
    id          bigserial   PRIMARY KEY,
    created_by  bigint      NOT NULL REFERENCES users (id),
    created_at  timestamptz NOT NULL DEFAULT now(),
    changed_by  bigint      NOT NULL REFERENCES users (id),
    changed_at  timestamptz NOT NULL DEFAULT now()
    class_name  text        NOT NULL,
    school_year text        NOT NULL
);


CREATE TABLE IF NOT EXISTS topic (
    id          bigserial   PRIMARY KEY,
    created_by  bigint      NOT NULL REFERENCES users (id),
    created_at  timestamptz NOT NULL DEFAULT now(),
    changed_by  bigint      NOT NULL REFERENCES users (id),
    changed_at  timestamptz NOT NULL DEFAULT now(),
    title       text        NOT NULL,
    description text        NOT NULL,
    image       url         NOT NULL,
    idea_space  bigint      REFERENCES idea_space (id),  -- 'NULL' == 'SchoolSpace'
    phase       phase       NOT NULL
);

CREATE TYPE phase AS ENUM
    ('edit_topics', 'feasibility', 'vote', 'finished');


----------------------------------------------------------------------
-- user

-- FIXME: who is the creator of the first user?  (she could create
-- herself.  it's philosophically confusing, and may be technically
-- un-tidy because we need the AUID in order to allocate it, but if
-- it's really the first user, it's easy enough to come up with a
-- fresh AUID.)
CREATE TABLE IF NOT EXISTS users (
    id         bigserial     PRIMARY KEY,
    created_by bigint        NOT NULL REFERENCES users (id),
    created_at timestamptz   NOT NULL DEFAULT now(),
    changed_by bigint        NOT NULL REFERENCES users (id),
    changed_at timestamptz   NOT NULL DEFAULT now(),
    login      text          NOT NULL,
    first_name text          NOT NULL,
    last_name  text          NOT NULL,
    avatar     text          NOT NULL,
        -- avatar images (png, jpg) can be uploaded to aula.  this
        -- table field can be used to construct the URL under which
        -- the image can be downloaded.
    password   text          NOT NULL,
    email      text
        -- FIXME: is there a postgresql email type?
);

-- FIXME: should we store pngs as blobs in postgres or on the file
-- system?

CREATE TABLE IF NOT EXISTS user_group (
    user_id      bigint    NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    group_id     group_id  NOT NULL,
    school_class bigint    REFERENCES school_class (id),

    UNIQUE (user_id, group_id, school_class)
    CHECK (group_id in 'student', 'class_guest') = (school_class IS NOT NULL)
);

CREATE TYPE group_id AS ENUM
    ('student', 'class_guest', 'school_guest', 'moderator', 'principal', 'admin');

CREATE TABLE IF NOT EXISTS delegation (
    id                 bigserial   PRIMARY KEY,
    created_by         bigint      NOT NULL REFERENCES users (id),
    created_at         timestamptz NOT NULL DEFAULT now(),
    changed_by         bigint      NOT NULL REFERENCES users (id),
    changed_at         timestamptz NOT NULL DEFAULT now()
    context_idea_space bigint      REFERENCES idea_space (id),
        -- 'NULL' == 'SchoolSpace'
    context_topic      bigint      REFERENCES topic (id),
    context_idea       bigint      REFERENCES idea (id),
    from_user          bigint      NOT NULL REFERENCES users (id),
    to_user            bigint      NOT NULL REFERENCES users (id),

    -- FIXME: CONSTRAINT: at most one of context_idea_space,
    -- context_topic, context_idea is not null.
);
