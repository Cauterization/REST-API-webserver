CREATE TABLE IF NOT EXISTS users ( 

    id          SERIAL PRIMARY KEY, 

    firstname   TEXT NOT NULL,  

    lastname    TEXT NOT NULL, 

    login       TEXT NOT NULL UNIQUE, 

    password    TEXT NOT NULL, 

    created     DATE NOT NULL DEFAULT CURRENT_DATE, 

    admin       BOOL  NOT NULL DEFAULT FALSE

); 


CREATE TABLE IF NOT EXISTS user_token ( 

    user_id     INTEGER REFERENCES users
                ON DELETE CASCADE, 

    token       TEXT NOT NULL UNIQUE

);

CREATE OR REPLACE VIEW users_view AS (

    SELECT users.id         AS id, 
           users.firstname  AS firstname, 
           users.lastname   AS lastname, 
           users.login      AS login,
           users.password   AS password, 
           users.created    AS created, 
           users.admin      AS admin,
           user_token.token AS token

    FROM users 
        INNER JOIN user_token ON users.id = user_id 

);

CREATE TABLE IF NOT EXISTS authors ( 

	id          SERIAL PRIMARY KEY, 

	userlink    INTEGER REFERENCES users
                ON DELETE CASCADE, 

	description TEXT NOT NULL 

);

CREATE OR REPLACE VIEW authors_view AS (

    SELECT authors.id  AS author_id,
           userlink    AS author_userlink,
           description AS author_description,
           users.id    AS user_id,
           firstname   AS user_firstname,
           lastname    AS user_lastname,
           login       AS user_login,
           created     AS user_created,
           admin       AS user_admin

    FROM authors
        INNER JOIN users ON authors.userlink = users.id

);

CREATE TABLE IF NOT EXISTS tags ( 

	id          SERIAL NOT NULL PRIMARY KEY, 

	name        TEXT NOT NULL UNIQUE 

);

CREATE TABLE IF NOT EXISTS categories ( 

    id          SERIAL PRIMARY KEY, 

	name        TEXT NOT NULL UNIQUE, 

	parent_id   INTEGER REFERENCES categories
                ON DELETE SET NULL

);


CREATE TABLE IF NOT EXISTS posts (

    id          SERIAL PRIMARY KEY, 

    title       TEXT NOT NULL UNIQUE, 

    content     TEXT NOT NULL,

    created     DATE NOT NULL 
                DEFAULT CURRENT_DATE, 

    author_id   INTEGER 
                REFERENCES authors
                ON DELETE CASCADE,

    category_id INTEGER
                REFERENCES categories
                ON DELETE SET NULL

); 


CREATE TABLE IF NOT EXISTS post_tag (

    post_id     INTEGER
                REFERENCES posts
                ON DELETE CASCADE, 

    tag_id      INTEGER
                REFERENCES tags
                ON DELETE CASCADE
                
);
             