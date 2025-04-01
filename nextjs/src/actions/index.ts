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
    // This is biz logic
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
  // This is biz logic
  if (!me) {
    posts = await infra.getPublishedPosts(); // IO
  } else if (me.role == UserRole.Admin) {
    posts = await infra.getAllPosts(); // IO
  } else {
    posts = await infra.getMyPostsOrPublishedPosts(me); // IO
  }

  return posts;
}
