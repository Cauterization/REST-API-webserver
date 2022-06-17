DROP VIEW authors_view CASCADE;

ALTER TABLE users RENAME COLUMN created TO registered;

CREATE OR REPLACE VIEW authors_view AS (

    SELECT authors.id  AS id,
           description,
           users.id    AS user_id,
           firstname,
           lastname,
           login,
           registered,
           admin,
           token,
           password

    FROM authors
        INNER JOIN users ON authors.user_id = users.id

);

ALTER TABLE users ALTER COLUMN token SET NOT NULL;