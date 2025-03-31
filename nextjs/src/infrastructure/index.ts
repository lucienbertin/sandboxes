"use server";
import { Place, Post, User, UserRole } from "@/domain";
import * as typeorm from "typeorm";
import "reflect-metadata";
import { DataSource, DeepPartial } from "typeorm";
import { Feature, FeatureCollection, Point } from "geojson";

interface IRecord<T> {
  asRecord(): T;
}

@typeorm.Entity({ name: "users" })
class ORMUser implements IRecord<User> {
  @typeorm.PrimaryGeneratedColumn()
  id!: number;

  @typeorm.Column({ type: "text", name: "first_name" })
  firstName!: string;

  @typeorm.Column({ type: "text", name: "last_name" })
  lastName!: string;

  @typeorm.Column({ type: "text" })
  email!: string;

  @typeorm.Column({
    type: "enum",
    enum: UserRole,
    default: UserRole.Reader,
  })
  role!: UserRole;

  @typeorm.OneToMany(() => ORMPost, (post) => post.author)
  posts!: ORMPost[];

  asRecord(): User {
    return {
      id: this.id,
      firstName: this.firstName,
      lastName: this.lastName,
      email: this.email,
      role: this.role,
    };
  }
}

@typeorm.Entity({ name: "posts" })
class ORMPost implements IRecord<Post> {
  @typeorm.PrimaryGeneratedColumn()
  id!: number;

  @typeorm.Column({ type: "text" })
  title!: string;

  @typeorm.Column({ type: "text" })
  body!: string;

  @typeorm.Column({ type: "boolean" })
  published!: boolean;

  @typeorm.ManyToOne(() => ORMUser)
  @typeorm.JoinColumn({ name: "author_id" })
  author!: ORMUser;

  asRecord(): Post {
    return {
      id: this.id,
      title: this.title,
      body: this.body,
      published: this.published,
      author: this.author.asRecord(),
    } as Post;
  }
}

@typeorm.Entity({ name: "places" })
class ORMPlace implements IRecord<Place> {
  @typeorm.PrimaryGeneratedColumn()
  id!: number;

  @typeorm.Column({ type: "text" })
  name!: string;

  @typeorm.Column({
    type: "geometry",
    srid: 4326,
    spatialFeatureType: "Point",
  })
  geometry!: typeorm.Point;

  asRecord(): Place {
    return { ...this } as Place;
  }
  asGeoJSON(): Feature<Point, Place> {
    return {
      id: this.id,
      type: "Feature",
      properties: this.asRecord(),
      geometry: this.geometry,
    };
  }
}

const datasource = new DataSource({
  type: "postgres",
  host: "localhost",
  port: 5432,
  password: "postgres",
  username: "postgres",
  database: "postgres",
  synchronize: false,
  logging: false,
  entities: [ORMPost, ORMPlace, ORMUser],
});

const isInitialized = datasource.initialize().then((ds) => ds.isInitialized);

export async function getPublishedPosts(): Promise<Post[]> {
  await isInitialized;
  const posts = await datasource.getRepository(ORMPost).find({
    where: { published: true },
    relations: { author: true },
  });
  return posts.map((p) => p.asRecord());
}
export async function getMyPostsOrPublishedPosts(me: User): Promise<Post[]> {
  await isInitialized;
  const posts = await datasource.getRepository(ORMPost).find({
    where: [{ published: true }, { author: { id: me.id } }],
    relations: { author: true },
  });
  return posts.map((p) => p.asRecord());
}
export async function getAllPosts(): Promise<Post[]> {
  await isInitialized;
  const posts = await datasource.getRepository(ORMPost).find({
    relations: { author: true },
  });
  return posts.map((p) => p.asRecord());
}

export async function getPost(id: number): Promise<Post | null> {
  await isInitialized;
  const post = await datasource.getRepository(ORMPost).findOne({
    where: { id },
    relations: { author: true },
  });

  return post?.asRecord() as Post | null;
}

export async function getPlaces(): Promise<Place[]> {
  await isInitialized;
  const places = await datasource.getRepository(ORMPlace).find();
  return places.map((p) => p.asRecord());
}

export async function getPlacesGeoJSON(): Promise<
  FeatureCollection<Point, Place>
> {
  await isInitialized;
  const places = await datasource.getRepository(ORMPlace).find();
  await waitforme(10000);
  return {
    type: "FeatureCollection",
    features: places.map((p) => p.asGeoJSON()),
  };
}

function waitforme(millisec: number) {
  return new Promise((resolve) => {
    setTimeout(() => {
      resolve("");
    }, millisec);
  });
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

export async function createPlace(
  newPlace: Feature<Point, DeepPartial<Place>>,
) {
  await isInitialized;

  const repo = datasource.getRepository(ORMPlace);
  const place = repo.create({ ...newPlace.properties });
  place.geometry = newPlace.geometry;

  await repo.save(place);
}

export async function authenticateUser(email: string): Promise<User | null> {
  await isInitialized;

  const repo = datasource.getRepository(ORMUser);
  const user = await repo.findOneBy({ email: email });

  return user;
}

export async function getUserByEmail(email: string): Promise<User | null> {
  await isInitialized;

  const repo = datasource.getRepository(ORMUser);
  const user = await repo.findOneBy({ email: email });

  return user;
}
