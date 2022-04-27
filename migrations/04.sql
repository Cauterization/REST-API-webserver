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
