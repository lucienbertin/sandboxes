import { AgentType, Place, Post, User, UserRole } from "@/domain";
import * as typeorm from "typeorm";
import "reflect-metadata";
import { DataSource } from "typeorm";
import { Feature, Point } from "geojson";

interface IRecord<T> {
  asRecord(): T;
}

@typeorm.Entity("user")
export class ORMUser implements IRecord<User> {
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

  asRecord(): User {
    return {
      _type: AgentType.User,
      id: this.id,
      firstName: this.firstName,
      lastName: this.lastName,
      email: this.email,
      role: this.role,
    };
  }
}

@typeorm.Entity("post")
export class ORMPost implements IRecord<Post> {
  @typeorm.PrimaryColumn({type: "int" })
  id!: number;

  @typeorm.Column({ type: "text" })
  title!: string;

  @typeorm.Column({ type: "text" })
  body!: string;

  @typeorm.Column({ type: "text" })
  author!: string;

  asRecord(): Post {
    return {
      id: this.id,
      title: this.title,
      body: this.body,
      author: this.author,
    } as Post;
  }
}

@typeorm.Entity("place")
export class ORMPlace implements IRecord<Place> {
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

export const datasource = new DataSource({
  type: "postgres",
  host: process.env.DB_HOST,
  port: 5432,
  password: "nextjs",
  username: "nextjs",
  database: "nextjs-db",
  synchronize: true,
  logging: false,
  entities: [ORMPost, ORMPlace, ORMUser],
});
export const isInitialized = datasource
  .initialize()
  .then((ds) => ds.isInitialized);
