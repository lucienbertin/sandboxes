ALTER TABLE posts
ADD COLUMN author_email TEXT not null default 'john@d.oe';

UPDATE posts 
SET author_email = users.email
FROM users WHERE posts.author_id = users.id;

ALTER TABLE posts 
DROP COLUMN author_id;

ALTER TABLE posts
RENAME COLUMN author_email to author;

