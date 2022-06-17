CREATE TABLE IF NOT EXISTS pictures (

    id         SERIAL PRIMARY KEY,

    format     TEXT NOT NULL
               CHECK (format IN ('JPEG','GIF','PNG')),

    picture    BYTEA NOT NULL   

);

CREATE TABLE IF NOT EXISTS article_pic (

    article_id INTEGER REFERENCES articles
               ON DELETE CASCADE,

    picture_id INTEGER REFERENCES pictures
               ON DELETE CASCADE

);

INSERT INTO pictures (format, picture) (SELECT 'JPEG', pic FROM pics);

INSERT INTO article_pic (article_id, picture_id) (
    SELECT post_id, pictures.id
    FROM pics
    INNER JOIN pictures ON pictures.picture = pics.pic
    WHERE related = 'post'
);

INSERT INTO article_pic (article_id, picture_id) (
    SELECT draft_id, pictures.id
    FROM pics
    INNER JOIN pictures ON pictures.picture = pics.pic
    WHERE related = 'draft'
);

DROP TABLE IF EXISTS pics                CASCADE;
DROP VIEW  IF EXISTS draft_pic_main_view CASCADE;
DROP VIEW  IF EXISTS draft_pic_sub_view  CASCADE;
DROP VIEW  IF EXISTS post_pic_main_view  CASCADE;
DROP VIEW  IF EXISTS post_pic_sub_view   CASCADE;


