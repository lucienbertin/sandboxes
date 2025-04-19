"use server";
import * as domain from "@/domain";
import * as infra from "@/infrastructure";

export const getPost = domain.getPost(infra.resolveAgent, infra.getPost);
export const getPosts = domain.getPosts(infra.resolveAgent, infra.getPosts);
export const getPostsCount = domain.countPosts(
  infra.resolveAgent,
  infra.countPosts,
);
export const createPost = domain.createPost(
  infra.resolveAgent,
  infra.createPost,
);
