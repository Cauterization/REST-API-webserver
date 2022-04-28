CREATE OR REPLACE FUNCTION post_draft 
    ( title_var    Text
    , created_var  DATE
    , content_var  TEXT
    , author_var   INTEGER
    , category_var INTEGER
    , tags_var     INTEGER []
    , pics_var     INTEGER []
    ) 
RETURNS INTEGER AS $draft_id$
DECLARE 
    draft_id_var INTEGER;
BEGIN

    INSERT INTO articles (title, created, content, author, category, published) 
        VALUES (title_var, created_var, content_var, author_var, category_var, false)
        RETURNING id INTO draft_id_var;

    INSERT INTO article_tag (tag_id, article_id)
        ( SELECT unnest, draft_id_var FROM UNNEST(tags_var)
        );

    RETURN draft_id_var;

END; 
$draft_id$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION put_draft 
    ( title_var    Text
    , created_var  DATE
    , content_var  TEXT
    , author_var   INTEGER
    , category_var INTEGER
    , tags_var     INTEGER []
    , pics_var     INTEGER []
    , draft_id_var INTEGER
    ) 
RETURNS INT AS $rows_affected$
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

    RETURN (SELECT COUNT(id) FROM articles WHERE id = draft_id_var);

END; 
$rows_affected$ LANGUAGE plpgsql;


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

