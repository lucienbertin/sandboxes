"use server";
import { Post, UserRole } from "@/domain";
import * as infra from "@/infrastructure";

export async function getPost(postId: number): Promise<Post> {
  const agent = await infra.resolveAgent(); // IO

  const post = await infra.getPost(postId); // IO

  // Domain logic
  if (!post) {
    return Promise.reject(new Error("post not found"));
  }
  if (!post.published && post.author.id != agent?.id) {
    return Promise.reject(new Error("insufficient rights"));
  }

  return post;
}

export async function getPosts(): Promise<Post[]> {
  const agent = await infra.resolveAgent(); // IO

  let posts = [];
  // Domain logic
  if (!agent) {
    posts = await infra.getPublishedPosts(); // IO
  } else if (agent.role == UserRole.Admin) {
    posts = await infra.getAllPosts(); // IO
  } else {
    posts = await infra.getMyPostsOrPublishedPosts(agent); // IO
  }

  return posts;
}

export async function getPostsCount(): Promise<number> {
  const agent = await infra.resolveAgent(); // IO

  let cnt;
  // Domain logic
  if (!agent) {
    cnt = await infra.getPublishedPostsCount(); // IO
  } else if (agent.role == UserRole.Admin) {
    cnt = await infra.getAllPostsCount(); // IO
  } else {
    cnt = await infra.getMyPostsOrPublishedPostsCount(agent); // IO
  }

  return cnt;
}

export async function createPost(post: Partial<Post>) {
  const agent = await infra.resolveAgent(); // IO

  if (!agent || agent.role == UserRole.Reader) {
    // Domain logic
    return Promise.reject(new Error("insufficient rights"));
  }

  await infra.createPost(post, agent); // IO
}
