CREATE DATABASE ai_labar;
\c ai_labar
-- Crear la tabla "groups" en PostgreSQL
CREATE TABLE groups (
    id SERIAL PRIMARY KEY,
    group_name VARCHAR NOT NULL,
    admin VARCHAR NOT NULL
);

-- Crear la tabla "members" en PostgreSQL
CREATE TABLE members (
    id SERIAL PRIMARY KEY,
    group_id INTEGER NOT NULL,
    user_id INTEGER NOT NULL
);

-- Crear la tabla "options" en PostgreSQL
CREATE TABLE options (
    id SERIAL PRIMARY KEY,
    topic_id INTEGER NOT NULL,
    image VARCHAR,
    topic_option VARCHAR NOT NULL,
    votes INTEGER NOT NULL
);

-- Crear la tabla "topics" en PostgreSQL
CREATE TABLE topics (
    id SERIAL PRIMARY KEY,
    title VARCHAR NOT NULL,
    type VARCHAR NOT NULL CHECK (type IN ('TEXT_MULTIPLE', 'TEXT_SINGLE', 'AS', 'RATING', 'IMAGE_SINGLE', 'IMAGE_MULTIPLE')),
    question VARCHAR NOT NULL,
    author VARCHAR NOT NULL,
    group_id INTEGER NOT NULL,
    close_date VARCHAR,
    visits INTEGER NOT NULL DEFAULT 0,
    status VARCHAR NOT NULL DEFAULT '1'
);

-- Crear la tabla "users" en PostgreSQL
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    user_name VARCHAR NOT NULL UNIQUE,
    password VARCHAR NOT NULL,
    email VARCHAR NOT NULL UNIQUE,
    token VARCHAR NOT NULL,
    gender VARCHAR,
    photo VARCHAR,
    language VARCHAR,
    visualization VARCHAR NOT NULL DEFAULT 'Paginacion'
);

-- Crear la tabla "voted_by" en PostgreSQL
CREATE TABLE voted_by (
    id SERIAL PRIMARY KEY,
    topic_id INTEGER NOT NULL,
    user_id INTEGER NOT NULL
);

-- Crear la tabla "sqlite_sequence" en PostgreSQL
CREATE TABLE sqlite_sequence (
    name TEXT,
    seq TEXT
);
