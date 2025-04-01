"use server";
import { Post } from "@/domain";
import * as domain from "@/domain";
import * as infra from "@/infrastructure";

export async function getPost(postId: number): Promise<Post> {
  return domain.getPost(
    postId,
    infra.resolveAgent,
    infra.getPost,
  );
}

export async function getPosts(): Promise<Post[]> {
  return domain.getPosts(
    infra.resolveAgent,
    infra.getPosts
  );
}

export async function getPostsCount(): Promise<number> {
  return domain.countPosts(
    infra.resolveAgent,
    infra.countPosts
  );
}

export async function createPost(post: Partial<Post>) {
  return domain.createPost(
    post,
    infra.resolveAgent,
    infra.createPost,
  );
}
