import "reflect-metadata";
import { DataSource } from "typeorm";
import { IPost, Post } from "./post.entity";
import { IPlace, Place } from "./place.entity";

export const datasource = new DataSource({
    type: 'postgres',
    host: 'localhost',
    port: 5432,
    password: 'postgres',
    username: 'postgres',
    database: 'postgres',
    synchronize: false,
    logging: true,
    entities: [Post, Place],
})

export const isInitialized = datasource.initialize().then(ds => ds.isInitialized);

export async function getPublishedPosts(): Promise<IPost[]> {
    const posts = await datasource.getRepository(Post).findBy({ published: true });

    return posts.map(p => p.asStruct());
}
export async function getPlaces(): Promise<IPlace[]> {
    const places = await datasource.getRepository(Place).find();

    return places.map(p => p.asStruct());
}