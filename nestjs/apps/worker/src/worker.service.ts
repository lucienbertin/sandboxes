import { Inject, Injectable } from '@nestjs/common';
import { Pool } from 'pg';
import { Place, Post } from './models';
import { stdout } from 'process';
import { Feature, Point } from 'geojson';

@Injectable()
export class WorkerService {
  constructor(@Inject('PG_CONNECTION') private conn: Pool) {}

  async upsertPost(post: Post) {
    const author = post.author;
    stdout.write(`upserting author ${author.email} | `);
    const authorQuery = `
            INSERT INTO author ("firstName", "lastName", "email")
            VALUES ($1, $2, $3)
            ON CONFLICT (email)
            DO UPDATE SET
                "firstName" = EXCLUDED."firstName",
                "lastName" = EXCLUDED."lastName";
        `;
    await this.conn.query(authorQuery, [
      author.first_name,
      author.last_name,
      author.email,
    ]);
    stdout.write('done | ');

    stdout.write(`upserting post ${post.id} | `);
    const postQuery = `
            INSERT INTO post ("id", "title", "body", "authorEmail")
            VALUES ($1, $2, $3, $4)
            ON CONFLICT (id)
            DO UPDATE SET
                "title" = EXCLUDED."title",
                "body" = EXCLUDED."body",
                "authorEmail" = EXCLUDED."authorEmail";
        `;
    await this.conn.query(postQuery, [
      post.id,
      post.title,
      post.body,
      author.email,
    ]);
    stdout.write('done \n');
  }

  async deletePost(post: Post) {
    stdout.write(`deleting post ${post.id} | `);
    const query = `
            DELETE FROM post
            WHERE id = $1;
        `;
    await this.conn.query(query, [post.id]);
    stdout.write('done \n');
  }

  async upsertPlace(placeFeature: Feature<Point, Place>) {
    const place = placeFeature.properties;
    stdout.write(`upserting place ${place.id} | `);
    const query = `
            INSERT INTO place ("id", "name", "point")
            VALUES ($1, $2, $3)
            ON CONFLICT (id)
            DO UPDATE SET
                "name" = EXCLUDED."name",
                "point" = EXCLUDED."point";
        `;
    await this.conn.query(query, [place.id, place.name, placeFeature.geometry]);
    stdout.write('done \n');
  }
}
