CREATE TABLE IF NOT EXISTS users (
    id bigserial PRIMARY KEY,
    name text NOT NULL,
    password text NOT NULL, -- byte string or something?
    email text
)
