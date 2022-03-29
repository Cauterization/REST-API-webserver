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