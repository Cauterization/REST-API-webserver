ALTER TABLE posts RENAME TO articles;

ALTER TABLE post_tag RENAME TO article_tag;

ALTER TABLE article_tag RENAME COLUMN post_id to article_id;

DROP VIEW authors_view CASCADE;

ALTER TABLE users RENAME COLUMN created TO registered;

ALTER TABLE articles RENAME COLUMN author_id TO author;

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

ALTER TABLE articles ADD COLUMN IF NOT EXISTS published bool default false;

UPDATE articles SET published = true;

INSERT INTO articles (title, content, created, author, category_id, published) 
SELECT title, content, created, author_id, category_id, false FROM drafts;

ALTER TABLE tags RENAME COLUMN name TO tag;
ALTER TABLE articles RENAME COLUMN category_id TO category;

CREATE OR REPLACE VIEW articles_view AS (

    SELECT AR.id                  AS id, 
           AR.title               AS title, 
           AR.created             AS created, 
           AR.content             AS content,
           AR.published           AS published,
       
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
         
           coalesce(branch, ARRAY['no_category']) AS category,
           coalesce(branch_id, ARRAY[0])          AS category_id,
  
           array_agg(tags.tag)    AS tags_names,
           array_agg(tags.id)     AS tags_id,
  
           pic                    AS pic,
           pics                   AS pics

    FROM articles AR
    	INNER JOIN authors_view          ON authors_view.id = AR.author
        LEFT JOIN cat_branches           ON AR.category = cat_branches.id
        LEFT  JOIN article_tag           ON article_tag.article_id = AR.id 
        LEFT  JOIN tags                  ON article_tag.tag_id  = tags.id 
        LEFT  JOIN post_pic_main_view PM ON PM.post_id = AR.id
        LEFT  JOIN post_pic_sub_view  PS ON PS.post_id = AR.id

    GROUP BY AR.id, AR.title, AR.created, AR.content, 
             authors_view.id, description, 
             user_id, firstname, lastname, login, 
             token, registered, admin, 
             branch, branch_id,
             pic, pics
);

ALTER TABLE authors ADD CONSTRAINT user_id_constraint UNIQUE(user_id);

ALTER TABLE users ALTER COLUMN token SET NOT NULL;

DROP TABLE drafts CASCADE;

UPDATE article_tag SET tag_id = (SELECT tag_id FROM draft_tag WHERE draft_id = article_id);

DROP TABLE draft_tag;

CREATE OR REPLACE VIEW articles_view AS (

    SELECT AR.id                  AS id, 
           AR.title               AS title, 
           AR.created             AS created, 
           AR.content             AS content,
           AR.published           AS published,

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

           branch                 AS category,
           branch_id              AS category_id,

           array_agg(tags.tag)    AS tags_names,
           array_agg(tags.id)     AS tags_id,
  
           pic                    AS pic,
           pics                   AS pics

    FROM articles AR
        INNER JOIN authors_view          ON authors_view.id = AR.author
        LEFT  JOIN cat_branches          ON AR.category = cat_branches.id
        LEFT  JOIN article_tag           ON article_tag.article_id = AR.id 
        LEFT  JOIN tags                  ON article_tag.tag_id  = tags.id 
        LEFT  JOIN post_pic_main_view PM ON PM.post_id = AR.id
        LEFT  JOIN post_pic_sub_view  PS ON PS.post_id = AR.id


     GROUP BY AR.id, AR.title, AR.created, AR.content, 
             authors_view.id, description, 
             user_id, firstname, lastname, login, 
             token, registered, admin, 
             branch, branch_id,
             pic, pics

);

ALTER TABLE categories ADD CONSTRAINT parent_key_constraint CHECK (id != parent);