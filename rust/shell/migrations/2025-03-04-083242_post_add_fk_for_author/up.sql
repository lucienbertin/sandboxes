ALTER TABLE posts
RENAME COLUMN author to author_email;

ALTER TABLE posts 
ADD COLUMN author_id INT NOT NULL DEFAULT 1 CONSTRAINT posts_author_fk REFERENCES users(id) ON DELETE RESTRICT;

UPDATE posts 
SET author_id = users.id
FROM users WHERE posts.author_email = users.email;

ALTER TABLE posts
DROP COLUMN author_email;