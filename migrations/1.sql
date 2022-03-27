CREATE OR REPLACE VIEW cat_branches AS (
    WITH RECURSIVE cats AS ( 
	    SELECT id, ARRAY [name] AS branch, name AS last, ARRAY [id] AS branch_id
  	    FROM categories 
  	    WHERE parent_id IS NULL 
      UNION 
	    SELECT child.id, branch || child.name, child.name, branch_id || child.id
	    FROM categories child 
	    INNER JOIN cats ON cats.id = child.parent_id 
	) 
    SELECT branch, last, id,  branch_id from cats
    ORDER BY branch
);

CREATE OR REPLACE VIEW cat_descendants AS (
    WITH RECURSIVE cats AS ( 
	    SELECT id, name AS parent, NULL as descendant
  	    FROM categories 
      UNION 
	    SELECT child.id, cats.parent, child.name
	    FROM categories child 
	    INNER JOIN cats ON cats.id = child.parent_id 
	) 
    SELECT parent, descendant from cats 
    WHERE descendant IS NOT NULL
    ORDER BY parent
);

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
           array_agg(tags.id)   AS tags_id

    FROM posts P 
    	INNER JOIN authors_view ON authors_view.author_id = P.author_id
        INNER JOIN cat_branches ON P.category_id = cat_branches.id
        LEFT  JOIN post_tag     ON post_tag.post_id = P.id 
        LEFT  JOIN tags         ON post_tag.tag_id  = tags.id 

    GROUP BY P.id, P.title, P.created, P.content, 
             authors_view.author_id, author_userlink, author_description, 
             user_id, user_firstname, user_lastname, user_login, user_created, user_admin, 
             branch, branch_id

);
