'use server';
import { Place, Post } from '@/domain';
import * as typeorm from 'typeorm';
import "reflect-metadata";
import { DataSource, DeepPartial } from "typeorm";
import { Feature, FeatureCollection, Point } from "geojson";

@typeorm.Entity({ name: 'posts' })
class ORMPost implements Post {
  @typeorm.PrimaryGeneratedColumn()
  id!: number;

  @typeorm.Column({ type: 'text' })
  title!: string;

  @typeorm.Column({ type: 'text' })
  body!: string;

  @typeorm.Column({ type: 'boolean' })
  published!: boolean;

  asStruct(): Post {
    return { ...this } as Post;
  }
}

@typeorm.Entity({ name: 'places' })
class ORMPlace implements Place {
  @typeorm.PrimaryGeneratedColumn()
  id!: number;

  @typeorm.Column({ type: 'text' })
  name!: string;

  @typeorm.Column({
    type: 'geometry',
    srid: 4326,
    spatialFeatureType: 'Point',
  })
  geometry!: typeorm.Point;

  asStruct(): Place {
    return { ...this } as Place;
  }
  asGeoJSON(): Feature<Point, Place> {
    return {
      id: this.id,
      type: 'Feature',
      properties: this.asStruct(),
      geometry: this.geometry,
    }
  }
}

const datasource = new DataSource({
    type: 'postgres',
    host: 'localhost',
    port: 5432,
    password: 'postgres',
    username: 'postgres',
    database: 'postgres',
    synchronize: false,
    logging: true,
    entities: [ORMPost, ORMPlace],
})

const isInitialized = datasource.initialize().then(ds => ds.isInitialized);

export async function getPublishedPosts(): Promise<Post[]> {
    await isInitialized;
    const posts = await datasource.getRepository(ORMPost).findBy({ published: true });
    return posts.map(p => p.asStruct());
}

export async function getPost(id: number): Promise<Post | null> {
    await isInitialized;
    const post = await datasource.getRepository(ORMPost).findOneBy({id});
    return post?.asStruct() as Post | null;
}

export async function getPlaces(): Promise<Place[]> {
    await isInitialized;
    const places = await datasource.getRepository(ORMPlace).find();
    return places.map(p => p.asStruct());
}

export async function getPlacesGeoJSON(): Promise<FeatureCollection<Point, Place>> {
    await isInitialized;
    const places = await datasource.getRepository(ORMPlace).find();
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

export async function createPost(newPost: DeepPartial<Post>) {
    await isInitialized;
    const repo = datasource.getRepository(ORMPost);
    const post = repo.create(newPost as DeepPartial<Post>);

    await repo.save(post);
}

export async function getPostsCount() {
    await isInitialized;
    const repo = datasource.getRepository(ORMPost);

    const cnt = await repo.count();
    await waitforme(1000);

    return cnt;
}

export async function createPlace(newPlace: Feature<Point, DeepPartial<Place>>) {
    await isInitialized;

    const repo = datasource.getRepository(ORMPlace);
    const place = repo.create({ ...newPlace.properties });
    place.geometry = newPlace.geometry;

    await repo.save(place);

}