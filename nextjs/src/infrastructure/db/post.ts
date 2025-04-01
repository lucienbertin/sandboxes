"use server";
import "reflect-metadata";
import { DeepPartial } from "typeorm";
import { Post, User } from "@/domain";
import { datasource, isInitialized, ORMPost, ORMUser } from "./datasource";

export async function getPublishedPosts(): Promise<Post[]> {
  await isInitialized;
  const posts = await datasource.getRepository(ORMPost).find({
    where: { published: true },
    relations: { author: true },
  });
  return posts.map((p) => p.asRecord());
}
export async function getPublishedPostsCount(): Promise<number> {
  await isInitialized;
  const cnt = await datasource.getRepository(ORMPost).count({
    where: { published: true },
  });
  return cnt;
}

export async function getMyPostsOrPublishedPosts(me: User): Promise<Post[]> {
  await isInitialized;
  const posts = await datasource.getRepository(ORMPost).find({
    where: [{ published: true }, { author: { id: me.id } }],
    relations: { author: true },
  });
  return posts.map((p) => p.asRecord());
}
export async function getMyPostsOrPublishedPostsCount(
  me: User,
): Promise<number> {
  await isInitialized;
  const cnt = await datasource.getRepository(ORMPost).count({
    where: [{ published: true }, { author: { id: me.id } }],
    relations: { author: true },
  });
  return cnt;
}

export async function getAllPosts(): Promise<Post[]> {
  await isInitialized;
  const posts = await datasource.getRepository(ORMPost).find({
    relations: { author: true },
  });
  return posts.map((p) => p.asRecord());
}
export async function getAllPostsCount(): Promise<number> {
  await isInitialized;
  const repo = datasource.getRepository(ORMPost);

  const cnt = await repo.count();

  return cnt;
}

export async function getPost(id: number): Promise<Post | null> {
  await isInitialized;
  const post = await datasource.getRepository(ORMPost).findOne({
    where: { id },
    relations: { author: true },
  });

  return post?.asRecord() as Post | null;
}

export async function createPost(newPost: DeepPartial<Post>, author: User) {
  await isInitialized;
  const repo = datasource.getRepository(ORMPost);
  const post = repo.create(newPost as DeepPartial<Post>);

  const ormAuthor = await datasource
    .getRepository(ORMUser)
    .findOneByOrFail({ id: author.id });
  post.author = ormAuthor;

  await repo.save(post);
}
