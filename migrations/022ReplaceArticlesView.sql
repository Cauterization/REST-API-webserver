DROP VIEW articles_view; 

CREATE OR REPLACE VIEW articles_view AS (

    SELECT AR.id                  AS id, 
           AR.title               AS title, 
           AR.created             AS created, 
           AR.content             AS content,

           authors_view.id        AS author_id, 
           description,
       
           user_id, 
           firstname, 
           lastname,  
           login, 
           token,
           null                   AS password,
           registered, 
           admin,
           
           cat_branches.last      AS cat_last,
           cat_branches.id        AS cat_id,
           branch                 AS cat_branch,
           branch_id              AS cat_branch_id,

           array_agg(tags.tag)    AS tags_names,
           array_agg(tags.id)     AS tags_id,
  
           array_agg(picture_id)  AS pics,

           published

    FROM articles AR
        INNER JOIN authors_view          ON authors_view.id = AR.author
        INNER JOIN cat_branches          ON AR.category = cat_branches.id
        LEFT  JOIN article_tag           ON article_tag.article_id = AR.id 
        LEFT  JOIN tags                  ON article_tag.tag_id  = tags.id 
        LEFT  JOIN article_picture       ON article_picture.article_id = AR.id

     GROUP BY AR.id, AR.title, AR.created, AR.content, 
             authors_view.id, description, 
             user_id, firstname, lastname, login, 
             token, registered, admin, 
             cat_last, cat_id, cat_branch, cat_branch_id,
             published

);


      

