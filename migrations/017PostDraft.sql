CREATE OR REPLACE FUNCTION post_draft 
    ( title_var    TEXT
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