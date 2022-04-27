CREATE TABLE IF NOT EXISTS comments (

    id          SERIAL PRIMARY KEY, 

    comment     TEXT NOT NULL,

    created     DATE NOT NULL 
                DEFAULT CURRENT_DATE,

    post_id     INTEGER
                REFERENCES posts
                ON DELETE CASCADE, 

    user_id     INTEGER
                REFERENCES users
                ON DELETE CASCADE

);

CREATE OR REPLACE FUNCTION user_token_check(int, text) 
  RETURNS void
  LANGUAGE plpgsql AS 
$func$  
DECLARE
   ex BOOL := EXISTS (SELECT * FROM user_token WHERE user_id = $1 AND token = $2);
BEGIN  
   IF NOT ex 
   THEN RAISE EXCEPTION 'WRONG TOKEN %', $2;
   END IF;
END
$func$;