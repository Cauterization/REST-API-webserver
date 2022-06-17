DROP VIEW IF EXISTS posts_view;

CREATE OR REPLACE VIEW posts_view AS (

    SELECT P.id                 AS post_id, 
           P.title              AS post_title, 
           P.created            AS post_created, 
           P.content            AS post_content,
       
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
           branch_id            AS category_id,

           array_agg(tags.name) AS tags_names,
           array_agg(tags.id)   AS tags_id,

           pic                  AS pic,
           pics                 AS pics

    FROM posts P 
    	INNER JOIN authors_view          ON authors_view.author_id = P.author_id
        INNER JOIN cat_branches          ON P.category_id = cat_branches.id
        LEFT  JOIN post_tag              ON post_tag.post_id = P.id 
        LEFT  JOIN tags                  ON post_tag.tag_id  = tags.id 
        LEFT  JOIN post_pic_main_view PM ON PM.post_id = P.id
        LEFT  JOIN post_pic_sub_view  PS ON PS.post_id = P.id

    GROUP BY P.id, P.title, P.created, P.content, 
             authors_view.author_id, author_userlink, author_description, 
             user_id, user_firstname, user_lastname, user_login, user_created, user_admin, 
             branch, branch_id,
             pic, pics
);