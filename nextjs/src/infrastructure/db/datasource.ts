import {
  Place as DPlace,
  Post as DPost,
  User as DUser,
  UserRole,
} from "@/domain";
import * as typeorm from "typeorm";
import "reflect-metadata";
import { DataSource } from "typeorm";
import { Feature, Point } from "geojson";

interface IRecord<T> {
  asRecord(): T;
}

@typeorm.Entity()
export class User implements IRecord<DUser> {
  @typeorm.PrimaryGeneratedColumn()
  id!: number;

  @typeorm.Column({ type: "text" })
  firstName!: string;

  @typeorm.Column({ type: "text" })
  lastName!: string;

  @typeorm.Column({ type: "text" })
  email!: string;

  @typeorm.Column({
    type: "enum",
    enum: UserRole,
    default: UserRole.Reader,
  })
  role!: UserRole;

  @typeorm.OneToMany(() => Post, (post) => post.author)
  posts!: Post[];

  asRecord(): DUser {
    return {
      id: this.id,
      firstName: this.firstName,
      lastName: this.lastName,
      email: this.email,
      role: this.role,
    };
  }
}

@typeorm.Entity()
export class Post implements IRecord<DPost> {
  @typeorm.PrimaryGeneratedColumn()
  id!: number;

  @typeorm.Column({ type: "text" })
  title!: string;

  @typeorm.Column({ type: "text" })
  body!: string;

  @typeorm.Column({ type: "boolean" })
  published!: boolean;

  @typeorm.ManyToOne(() => User)
  author!: User;

  asRecord(): DPost {
    return {
      id: this.id,
      title: this.title,
      body: this.body,
      published: this.published,
      author: this.author.asRecord(),
    } as DPost;
  }
}

@typeorm.Entity()
export class Place implements IRecord<DPlace> {
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

  asRecord(): DPlace {
    return { ...this } as DPlace;
  }
  asGeoJSON(): Feature<Point, DPlace> {
    return {
      id: this.id,
      type: "Feature",
      properties: this.asRecord(),
      geometry: this.geometry,
    };
  }
}

export const datasource = new DataSource({
  type: "postgres",
  host: "localhost",
  port: 5432,
  password: "nextjs",
  username: "nextjs",
  database: "nextjs-db",
  synchronize: true,
  logging: false,
  entities: [Post, Place, User],
});
export const isInitialized = datasource
  .initialize()
  .then((ds) => ds.isInitialized);
