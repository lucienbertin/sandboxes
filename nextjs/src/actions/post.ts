"use server";
import { Post } from "@/domain";
import * as domain from "@/domain";
import { auth, db, web } from "@/infrastructure";

async function getALLPosts(scope: domain.PostScope, author: domain.User | null): Promise<domain.Post[]> {
  const dbPosts = await db.getPosts(scope, author);
  const webPosts = await web.getPosts();
  return dbPosts.concat(webPosts);
}

export async function getPost(postId: number): Promise<Post> {
  return domain.getPost(postId, auth.resolveAgent, db.getPost);
}

export async function getPosts(): Promise<Post[]> {
  return domain.getPosts(auth.resolveAgent, getALLPosts);
  // return domain.getPosts(auth.resolveAgent, web.getPosts);
  // return domain.getPosts(auth.resolveAgent, db.getPosts);
}

export async function getPostsCount(): Promise<number> {
  return domain.countPosts(auth.resolveAgent, db.countPosts);
}

export async function createPost(post: Partial<Post>) {
  return domain.createPost(post, auth.resolveAgent, db.createPost);
}
