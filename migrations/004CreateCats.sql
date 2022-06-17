CREATE TABLE IF NOT EXISTS categories ( 

    id          SERIAL PRIMARY KEY, 

	name        TEXT NOT NULL UNIQUE, 

	parent_id   INTEGER REFERENCES categories
                ON DELETE SET NULL

);

CREATE OR REPLACE VIEW cat_branches AS (
    WITH RECURSIVE cats AS ( 
	    SELECT id, ARRAY [name] AS branch, name AS last, ARRAY [id] AS branch_id
  	    FROM categories 
  	    WHERE parent_id IS NULL 
      UNION 
	    SELECT child.id, child.name || branch, child.name,  child.id || branch_id
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