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
