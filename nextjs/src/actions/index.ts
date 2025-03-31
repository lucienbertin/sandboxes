import { Post } from "@/domain";
import * as db from "@/infrastructure";
import { getServerSession } from "next-auth";

export async function getPost(postId: number): Promise<Post> {
    const session = await getServerSession();
    let me = null;
    if (session?.user?.email) {
        me = await db.getUserByEmail(session?.user?.email); // IO
    }

    const post = await db.getPost(postId); // IO

    if (!post) {
        return Promise.reject(new Error("post not found"));
    }
    if (!post.published && post.author.id != me?.id) {
        return Promise.reject(new Error("insufficient rights"));
    }

    return post;
}

export async function getPosts(): Promise<Post[]> {
    const session = await getServerSession();
    let me = null;
    if (session?.user?.email) {
        me = await db.getUserByEmail(session?.user?.email); // IO
    }

    let posts = [];
    if (!me) {
        posts = await db.getPublishedPosts(); // IO
    } else {
        posts = await db.getMyPostsOrPublishedPosts(me); // IO
    }

    return posts;
}