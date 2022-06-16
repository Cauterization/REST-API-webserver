CREATE OR REPLACE PROCEDURE put_draft 
    ( title_var    Text
    , content_var  TEXT
    , category_var INTEGER
    , tags_var     INTEGER []
    , pics_var     INTEGER []
    , draft_id_var INTEGER
    ) 
LANGUAGE plpgsql
AS $$
BEGIN
    UPDATE articles 
    SET title = COALESCE(title_var, title),
        content = COALESCE(content_var, content),
        category = COALESCE(category_var, category)
    WHERE id = draft_id_var;

    IF NOT (tags_var IS NULL) THEN
        DELETE FROM article_tag WHERE article_id = draft_id_var;
        INSERT INTO article_tag (SELECT draft_id_var, unnest FROM UNNEST(tags_var));
    END IF; 
    
END $$;