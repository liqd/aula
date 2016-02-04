CREATE TYPE idea_vote_value AS ENUM ('yes', 'no', 'neutral');

CREATE TYPE up_down AS ENUM ('up', 'down');

CREATE TYPE phase AS ENUM
    ('wild_ideas', 'edit_topics', 'feasibility', 'vote', 'finished');

CREATE TYPE category AS ENUM
    ('rule', 'equipment', 'class', 'time', 'environment');

CREATE TYPE group_id AS ENUM
    ('admin', 'moderator', 'principal', 'student', 'guest', 'in_class');

-- plural table name because 'user' is a sql keyword
-- TODO: problem: how creates the creators?
CREATE TABLE IF NOT EXISTS users (
    id bigserial PRIMARY KEY,
    login text NOT NULL,
    avatar text NOT NULL, -- url
    password bytea NOT NULL,
    email text NOT NULL,
    created_by bigint NOT NULL REFERENCES users (id),
    created_at timestamptz NOT NULL DEFAULT now(),
    changed_by bigint NOT NULL REFERENCES users (id),
    changed_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS school_class (
    id bigserial PRIMARY KEY,
    name text NOT NULL,
    school_year text NOT NULL
);
CREATE TABLE IF NOT EXISTS idea_space (
    id bigserial PRIMARY KEY,
    title text NOT NULL,
    class bigint REFERENCES school_class, -- NULL means ISSchol
    description text NOT NULL,
    -- TODO: _ideaSpaceWildIdeas, _ideaSpaceTopics
    topic bigint REFERENCES topic (id),
    created_by bigint NOT NULL REFERENCES users (id),
    created_at timestamptz NOT NULL DEFAULT now(),
    changed_by bigint NOT NULL REFERENCES users (id),
    changed_at timestamptz NOT NULL DEFAULT now()
);


CREATE TABLE IF NOT EXISTS idea (
    idea_space bigint NOT NULL REFERENCES idea_space (id),
    title text NOT NULL,
    description text NOT NULL, -- 'desc' is an sql keyword
    category category NOT NULL,
    phase phase NOT NULL,
    topic bigint REFERENCES topic (id),
    feasible bigint REFERENCES feasible (id),
    created_by bigint NOT NULL REFERENCES users (id),
    created_at timestamptz NOT NULL DEFAULT now(),
    changed_by bigint NOT NULL REFERENCES users (id),
    changed_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS topic (
    id bigserial PRIMARY KEY,
    name text NOT NULL,
    description text NOT NULL,
    image url NOT NULL
    created_by bigint NOT NULL REFERENCES users (id),
    created_at timestamptz NOT NULL DEFAULT now(),
    changed_by bigint NOT NULL REFERENCES users (id),
    changed_at timestamptz NOT NULL DEFAULT now()
);


CREATE TABLE IF NOT EXISTS feasible (
    id bigserial PRIMARY KEY,
    value bool NOT NULL,
    reason text NOT NULL
    created_by bigint NOT NULL REFERENCES users (id),
    created_at timestamptz NOT NULL DEFAULT now(),
    changed_by bigint NOT NULL REFERENCES users (id),
    changed_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS idea_like (
);

CREATE TABLE IF NOT EXISTS comment (
    id bigserial PRIMARY KEY,
    text text NOT NULL,
    parent_comment bigint REFERENCES comment (id),
    parent_idea bigint REFERENCES idea (id)
    created_by bigint NOT NULL REFERENCES users (id),
    created_at timestamptz NOT NULL DEFAULT now(),
    changed_by bigint NOT NULL REFERENCES users (id),
    changed_at timestamptz NOT NULL DEFAULT now()
    CHECK (parent_comment IS NULL) <> (parent_idea IS NULL)
);

CREATE TABLE IF NOT EXISTS comment_vote (
    comment bigint NOT NULL REFERENCES comment (id),
    value up_down NOT NULL
    created_by bigint NOT NULL REFERENCES users (id),
    created_at timestamptz NOT NULL DEFAULT now(),
    changed_by bigint NOT NULL REFERENCES users (id),
    changed_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS idea_vote (
    idea bigint NOT NULL REFERENCES idea (id),
    value idea_vote_value NOT NULL
    created_by bigint NOT NULL REFERENCES users (id),
    created_at timestamptz NOT NULL DEFAULT now(),
    changed_by bigint NOT NULL REFERENCES users (id),
    changed_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS user_group (
    user_id bigint REFERENCES users (id) ON DELETE CASCADE,
    group_id group_id,
    school_class bigint REFERENCES school_class (id),
    school_year text,
    UNIQUE (user_id, group_id, school_class, school_year)
    CHECK (group_id = 'in_class') = (schoolyear IS NOT NULL AND schoolclass IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS delegation (
    from bigint REFERENCES users (id),
    to bigint REFERENCES users (id),
    -- TODO _delegationIdeaSpace ?
    created_by bigint NOT NULL REFERENCES users (id),
    created_at timestamptz NOT NULL DEFAULT now(),
    changed_by bigint NOT NULL REFERENCES users (id),
    changed_at timestamptz NOT NULL DEFAULT now()
);
