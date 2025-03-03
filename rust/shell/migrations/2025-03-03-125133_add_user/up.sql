CREATE TYPE USER_ROLE AS ENUM ('reader', 'writer', 'admin');

CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  email text NOT NULL UNIQUE,
  role USER_ROLE NOT NULL DEFAULT 'reader'
)