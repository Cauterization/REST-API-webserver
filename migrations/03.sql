CREATE OR REPLACE VIEW drafts_view AS (

    SELECT D.id                 AS draft_id, 
           D.title              AS draft_title, 
           D.content            AS draft_content,
           D.created            AS draft_created,
           D.last_update        AS draft_last_update, 

           draft_token.token    AS token,

           authors_view.author_id,
           author_userlink,
           author_description,

           user_id, 
           user_firstname, 
           user_lastname,  
           user_login, 
           user_created, 
           user_admin,
       
           branch               AS category,

           array_agg(tags.name) AS tags

    FROM drafts D
        INNER JOIN draft_token  ON draft_token.draft_id = D.id
        INNER JOIN authors_view ON authors_view.author_id = D.author_id
        INNER JOIN cat_branches ON cat_branches.id = D.category_id 
        LEFT  JOIN draft_tag    ON draft_tag.draft_id = D.id
        LEFT  JOIN tags         ON tags.id = draft_tag.tag_id

    GROUP BY 
        D.id, D.title, D.created, D.last_update, D.content,
        draft_token.token,
        authors_view.author_id, author_userlink, author_description,
        user_id, user_firstname, user_lastname, user_login, user_created, user_admin,
        branch

);