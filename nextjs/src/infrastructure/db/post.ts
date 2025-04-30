"use server";
import "reflect-metadata";
import { Post } from "@/domain";
import { datasource, isInitialized, ORMPost as EPost } from "./datasource";

export async function getPosts(): Promise<Post[]> {
  await isInitialized;
  const posts = await datasource.getRepository(EPost).find();
  return posts.map((p) => p.asRecord());
}

export async function countPosts(): Promise<number> {
  await isInitialized;
  const cnt = await datasource.getRepository(EPost).count();
  return cnt;
}

export async function getPost(id: number): Promise<Post | null> {
  await isInitialized;
  const post = await datasource.getRepository(EPost).findOne({
    where: { id },
  });

  return post?.asRecord() as Post | null;
}

export async function upsertPost(post: Post) {
  await isInitialized;
  const repo = datasource.getRepository(EPost);

  await repo.upsert(post, {
    conflictPaths: {
      id: true,
    },
    skipUpdateIfNoValuesChanged: true,
    upsertType: "on-conflict-do-update",
  });
}

export async function deletePost(post: Post) {
  await isInitialized;
  const repo = datasource.getRepository(EPost);
  await repo.delete(post.id);
}
