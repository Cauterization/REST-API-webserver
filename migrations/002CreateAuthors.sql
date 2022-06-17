CREATE TABLE IF NOT EXISTS authors ( 

	id          SERIAL PRIMARY KEY, 

	userlink    INTEGER REFERENCES users
                ON DELETE CASCADE, 

	description TEXT NOT NULL 

);

DROP VIEW IF EXISTS authors_view;

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
