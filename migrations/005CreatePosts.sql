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
           array_agg(tags.id)   AS tags_id

    FROM posts P 
    	INNER JOIN authors_view ON authors_view.author_id = P.author_id
        INNER JOIN cat_branches ON P.category_id = cat_branches.id
        LEFT  JOIN post_tag     ON post_tag.post_id = P.id 
        LEFT  JOIN tags         ON post_tag.tag_id  = tags.id 

    GROUP BY P.id, P.title, P.created, P.content, 
             authors_view.author_id, author_userlink, author_description, 
             user_id, user_firstname, user_lastname, user_login, user_created, user_admin, 
             branch, branch_id

);

