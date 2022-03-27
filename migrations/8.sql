CREATE TABLE IF NOT EXISTS pics (

    id          SERIAL PRIMARY KEY,

    related     VARCHAR(5) 
                NOT NULL
                CHECK (related IN ('draft','post')),

    relative_id INTEGER
                NOT NULL,

    post_id     INTEGER
                REFERENCES posts
                ON DELETE CASCADE, 

    draft_id    INTEGER
                REFERENCES drafts
                ON DELETE CASCADE, 

    pic         BYTEA NOT NULL           
);

CREATE OR REPLACE VIEW draft_pic_main_view AS (
    SELECT relative_id AS pic, draft_id
    FROM pics
    WHERE related = 'draft' AND relative_id = 1
);

CREATE OR REPLACE VIEW draft_pic_sub_view AS (
    SELECT array_agg(relative_id) AS pics, draft_id
    FROM pics
    WHERE related = 'draft' AND relative_id != 1
    GROUP BY draft_id
);

CREATE OR REPLACE VIEW post_pic_main_view AS (
   SELECT relative_id AS pic, post_id
    FROM pics
    WHERE related = 'post' AND relative_id = 1
);

CREATE OR REPLACE VIEW post_pic_sub_view AS (
    SELECT array_agg(relative_id) AS pics, post_id
    FROM pics
    WHERE related = 'post' AND relative_id != 1
    GROUP BY post_id
);

DROP VIEW IF EXISTS drafts_view;

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

           array_agg(tags.name) AS tags,

           pic                  AS pic,
           pics                 AS pics

    FROM drafts D
        INNER JOIN draft_token            ON draft_token.draft_id = D.id
        INNER JOIN authors_view           ON authors_view.author_id = D.author_id
        INNER JOIN cat_branches           ON cat_branches.id = D.category_id 
        LEFT  JOIN draft_tag              ON draft_tag.draft_id = D.id
        LEFT  JOIN tags                   ON tags.id = draft_tag.tag_id
        LEFT  JOIN draft_pic_main_view DM ON DM.draft_id = D.id
        LEFT  JOIN draft_pic_sub_view  DS ON DS.draft_id = D.id
        
    GROUP BY 
        D.id, D.title, D.created, D.last_update, D.content,
        draft_token.token,
        authors_view.author_id, author_userlink, author_description,
        user_id, user_firstname, user_lastname, user_login, user_created, user_admin,
        branch,
        pic, pics
);

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
