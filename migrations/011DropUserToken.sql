
ALTER TABLE IF EXISTS users ADD COLUMN IF NOT EXISTS token TEXT;

UPDATE users SET token = (SELECT token FROM user_token WHERE user_id = id);

DROP TABLE IF EXISTS user_token CASCADE;