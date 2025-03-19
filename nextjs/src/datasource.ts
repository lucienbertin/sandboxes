'use server';
import "reflect-metadata";
import { DataSource, DeepPartial } from "typeorm";
import { IPost, Post } from "./post.entity";
import { IPlace, Place } from "./place.entity";
import { FeatureCollection, Point } from "geojson";

const datasource = new DataSource({
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

const isInitialized = datasource.initialize().then(ds => ds.isInitialized);

export async function getPublishedPosts(): Promise<IPost[]> {
    await isInitialized;
    const posts = await datasource.getRepository(Post).findBy({ published: true });
    return posts.map(p => p.asStruct());
}

export async function getPost(id: number): Promise<IPost | null> {
    await isInitialized;
    const post = await datasource.getRepository(Post).findOneBy({id});
    return post?.asStruct() as IPost | null;
}

export async function getPlaces(): Promise<IPlace[]> {
    await isInitialized;
    const places = await datasource.getRepository(Place).find();
    return places.map(p => p.asStruct());
}

export async function getPlacesGeoJSON(): Promise<FeatureCollection<Point, IPlace>> {
    await isInitialized;
    const places = await datasource.getRepository(Place).find();
    await waitforme(10000);
    return {
        type: "FeatureCollection",
        features: places.map(p => p.asGeoJSON()),
    }
}

function waitforme(millisec: number) {
    return new Promise(resolve => {
        setTimeout(() => { resolve('') }, millisec);
    })
}

export async function createPost(newPost: DeepPartial<IPost>) {
    await isInitialized;
    const repo = datasource.getRepository(Post);
    const post = repo.create(newPost as DeepPartial<Post>);

    await repo.save(post);
}

export async function getPostsCount() {
    await isInitialized;
    const repo = datasource.getRepository(Post);

    let cnt = await repo.count();
    await waitforme(1000);

    return cnt;
}