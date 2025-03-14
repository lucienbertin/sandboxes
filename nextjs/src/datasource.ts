import "reflect-metadata";
import { DataSource } from "typeorm";
import { IPost, Post } from "./post.entity";
import { IPlace, Place } from "./place.entity";
import { FeatureCollection, Point } from "geojson";

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

export async function getPost(id: number): Promise<IPost | null> {
    const post = await datasource.getRepository(Post).findOneBy({id});
    return post?.asStruct() as IPost | null;
}

export async function getPlaces(): Promise<IPlace[]> {
    const places = await datasource.getRepository(Place).find();
    return places.map(p => p.asStruct());
}

export async function getPlacesGeoJSON(): Promise<FeatureCollection<Point, IPlace>> {
    const places = await datasource.getRepository(Place).find();
    return {
        type: "FeatureCollection",
        features: places.map(p => p.asGeoJSON()),
    }
}