"use server";
import { Post, User } from "@/domain";

import "reflect-metadata";
import { datasource, isInitialized, ORMPost } from "./datasource";
import { DeepPartial } from "typeorm";


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

  return cnt;
}