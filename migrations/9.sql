DROP VIEW IF EXISTS authors_view CASCADE;

CREATE OR REPLACE VIEW authors_view AS (

    SELECT authors.id,
           description AS description,
           users_view.id AS user_id,
           firstname,
           lastname,
           login,
           token,
           password,
           created,
           admin

    FROM authors
        INNER JOIN users_view ON authors.userlink = users_view.id

);

ALTER TABLE IF EXISTS users ADD COLUMN IF NOT EXISTS token TEXT;

UPDATE users SET token = (SELECT token FROM user_token WHERE user_id = id);

DROP TABLE IF EXISTS user_token CASCADE;

ALTER TABLE authors RENAME COLUMN userlink TO user_id;
