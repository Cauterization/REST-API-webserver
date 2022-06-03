ALTER TABLE categories RENAME COLUMN parent_id TO parent;

CREATE OR REPLACE VIEW cat_parents AS (
    WITH RECURSIVE cats AS ( 
	    SELECT id , ARRAY []::Int[] as parents
  	    FROM categories 
        WHERE parent IS NULL 
      UNION 
	    SELECT child.id, (cats.id || parents)
	    FROM categories child 
	    INNER JOIN cats ON cats.id = child.parent
	) 
    SELECT id, parents from cats 
    WHERE parents != '{}'
);