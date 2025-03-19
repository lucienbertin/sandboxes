export const dynamic = 'force-static';

import { getPublishedPosts, createPost } from "@/datasource"
import { IPost } from "@/post.entity";
import { NextRequest } from "next/server";
import { DeepPartial } from "typeorm";

export async function GET() {
    const posts = await getPublishedPosts();

    return Response.json(posts);
}

export async function POST(request: NextRequest) {
    const requestBody = await request.json();
    const newPost: DeepPartial<IPost> = {
        title: requestBody.title,
        body: requestBody.body,
    };

    await createPost(newPost);

    return new Response(null, { status: 201});
}