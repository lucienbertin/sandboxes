"use server";
import { Post, UserRole } from "@/domain";
import * as infra from "@/infrastructure";
import { getServerSession } from "next-auth";

export async function getPost(postId: number): Promise<Post> {
  const session = await getServerSession();
  let me = null;
  if (session?.user?.email) {
    me = await infra.getUserByEmail(session?.user?.email); // IO
  }

  const post = await infra.getPost(postId); // IO

  if (!post) {
    return Promise.reject(new Error("post not found"));
  }
  if (!post.published && post.author.id != me?.id) {
    // Domain logic
    return Promise.reject(new Error("insufficient rights"));
  }

  return post;
}

export async function getPosts(): Promise<Post[]> {
  const session = await getServerSession();
  let me = null;
  if (session?.user?.email) {
    me = await infra.getUserByEmail(session?.user?.email); // IO
  }

  let posts = [];
  // Domain logic
  if (!me) {
    posts = await infra.getPublishedPosts(); // IO
  } else if (me.role == UserRole.Admin) {
    posts = await infra.getAllPosts(); // IO
  } else {
    posts = await infra.getMyPostsOrPublishedPosts(me); // IO
  }

  return posts;
}

export async function getPostsCount(): Promise<number> {
  const session = await getServerSession();
  let me = null;
  if (session?.user?.email) {
    me = await infra.getUserByEmail(session?.user?.email); // IO
  }

  let cnt;
  // Domain logic
  if (!me) {
    cnt = await infra.getPublishedPostsCount(); // IO
  } else if (me.role == UserRole.Admin) {
    cnt = await infra.getAllPostsCount(); // IO
  } else {
    cnt = await infra.getMyPostsOrPublishedPostsCount(me); // IO
  }

  return cnt;
}

export async function createPost(post: Partial<Post>) {
  const session = await getServerSession();
  let me = null;
  if (session?.user?.email) {
    me = await infra.getUserByEmail(session?.user?.email); // IO
  }

  if (!me || me.role == UserRole.Reader) { // Domain logic
    return Promise.reject(new Error("insufficient rights"));
  }

  await infra.createPost(post, me); // IO
}