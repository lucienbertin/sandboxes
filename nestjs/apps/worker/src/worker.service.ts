import { Inject, Injectable } from '@nestjs/common';
import { Pool } from 'pg';
import { Post } from './models';

@Injectable()
export class WorkerService {
    constructor(@Inject('PG_CONNECTION') private conn: Pool) {}

    async upsertPost(post: Post) {

        console.log("upserting author")
        const author = post.author;
        const authorQuery = `
            INSERT INTO author ("firstName", "lastName", "email")
            VALUES ($1, $2, $3)
            ON CONFLICT (email)
            DO UPDATE SET
                "firstName" = EXCLUDED."firstName",
                "lastName" = EXCLUDED."lastName";
        `;
        await this.conn.query(
            authorQuery,
            [author.first_name, author.last_name, author.email]
        );
        console.log("author upserted")
        
        console.log("upserting post")
        const postQuery = `
            INSERT INTO post ("id", "title", "body", "authorEmail")
            VALUES ($1, $2, $3, $4)
            ON CONFLICT (id)
            DO UPDATE SET
                "title" = EXCLUDED."title",
                "body" = EXCLUDED."body",
                "authorEmail" = EXCLUDED."authorEmail";
        `;
        await this.conn.query(
            postQuery,
            [post.id, post.title, post.body, author.email]
        );
        console.log("post upserted")
    }
}

