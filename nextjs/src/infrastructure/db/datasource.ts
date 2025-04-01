"use server";
import { Place, Post, User, UserRole } from "@/domain";
import * as typeorm from "typeorm";
import "reflect-metadata";
import { DataSource } from "typeorm";
import { Feature, Point } from "geojson";

interface IRecord<T> {
  asRecord(): T;
}

@typeorm.Entity({ name: "users" })
export class ORMUser implements IRecord<User> {
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
export class ORMPost implements IRecord<Post> {
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
  host: "localhost",
  port: 5432,
  password: "postgres",
  username: "postgres",
  database: "postgres",
  synchronize: false,
  logging: false,
  entities: [ORMPost, ORMPlace, ORMUser],
});
export const isInitialized = datasource.initialize().then((ds) => ds.isInitialized);
