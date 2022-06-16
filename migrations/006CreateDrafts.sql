CREATE TABLE IF NOT EXISTS drafts (

    id          SERIAL PRIMARY KEY, 

    title       TEXT NOT NULL, 

    content     TEXT NOT NULL,

    created     DATE NOT NULL DEFAULT CURRENT_DATE, 

    last_update DATE NOT NULL DEFAULT CURRENT_DATE, 

    author_id   INTEGER
                REFERENCES authors
                ON DELETE CASCADE,

    category_id INTEGER
                REFERENCES categories
                ON DELETE SET NULL
); 

CREATE TABLE IF NOT EXISTS draft_tag (

    draft_id    INTEGER
                REFERENCES drafts
                ON DELETE CASCADE, 

    tag_id      INTEGER
                REFERENCES tags
                ON DELETE CASCADE 

);

CREATE OR REPLACE VIEW draft_token AS (

    SELECT drafts.id AS draft_id, token

    FROM drafts
        INNER JOIN authors ON drafts.author_id = authors.id
        INNER JOIN user_token ON user_id = authors.userlink

);

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

CREATE OR REPLACE FUNCTION draft_access(text, int) 
  RETURNS void
  LANGUAGE plpgsql AS 
$func$  
DECLARE
   ex BOOL := EXISTS (SELECT * FROM drafts_view WHERE token = $1 AND draft_id = $2);
BEGIN  
   IF NOT ex 
   THEN RAISE EXCEPTION 'WRONG TOKEN %', $1 USING ERRCODE = '23505';
   END IF;
END
$func$;

CREATE OR REPLACE FUNCTION token_check (TEXT) 
  RETURNS INT AS $$
BEGIN  
   IF NOT EXISTS (SELECT user_id FROM user_token WHERE token = $1)
   THEN RAISE EXCEPTION 'TOKEN % DOES NOT EXISTS', $1;
   ELSE RETURN (SELECT user_id FROM user_token WHERE token = $1);
   END IF;
END; $$ LANGUAGE plpgsql;