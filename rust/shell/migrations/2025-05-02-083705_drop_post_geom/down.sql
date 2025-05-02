-- opposite of what happens in 2025-02-06-102719_add_point_to_posts
ALTER TABLE posts ADD COLUMN geom geometry(Point,4326);