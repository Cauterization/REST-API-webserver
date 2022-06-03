CREATE TABLE IF NOT EXISTS users ( 

    id          SERIAL PRIMARY KEY, 

    firstname   TEXT NOT NULL,  

    lastname    TEXT NOT NULL, 

    login       TEXT NOT NULL UNIQUE, 

    password    TEXT NOT NULL, 

    created     DATE NOT NULL DEFAULT CURRENT_DATE, 

    admin       BOOL  NOT NULL DEFAULT FALSE

); 


CREATE TABLE IF NOT EXISTS user_token ( 

    user_id     INTEGER REFERENCES users
                ON DELETE CASCADE, 

    token       TEXT NOT NULL UNIQUE

);

CREATE OR REPLACE VIEW users_view AS (

    SELECT users.id         AS id, 
           users.firstname  AS firstname, 
           users.lastname   AS lastname, 
           users.login      AS login,
           users.password   AS password, 
           users.created    AS created, 
           users.admin      AS admin,
           user_token.token AS token

    FROM users 
        INNER JOIN user_token ON users.id = user_id 

);