import { Post } from "@/domain";
import { getPost as dbGetPost, getUserByEmail as dbGetUserByEmail } from "@/infrastructure";
import { getServerSession } from "next-auth";

export async function getPost(postId: number): Promise<Post> {
    const session = await getServerSession();
    let agent = null;
    if (session?.user?.email) {
        agent = await dbGetUserByEmail(session?.user?.email);
    }

    const post = await dbGetPost(postId);

    if (!post) {
        return Promise.reject(new Error("post not found"));
    }

    return post;
}