CREATE OR REPLACE VIEW comments_view AS (

    SELECT C.id        AS comment_id,
           comment     AS comment_text,
           C.created   AS comment_created,
           C.post_id   AS post_id,
        
           U.id        AS user_id,
           U.firstname AS user_firstname,
           U.lastname  AS user_lastname,
           U.login     AS user_login,
           U.created   AS user_created,
           U.admin     AS user_admin

    FROM comments C
        INNER JOIN users U ON C.user_id = U.id

);
