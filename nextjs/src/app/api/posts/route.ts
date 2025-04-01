export const dynamic = "force-static";

import { getPosts, createPost } from "@/actions";
import { Post } from "@/domain";
import { NextRequest } from "next/server";

export async function GET() {
  const posts = await getPosts();

  return Response.json(posts);
}

export async function POST(request: NextRequest) {
  const requestBody = await request.json();
  const newPost: Partial<Post> = {
    title: requestBody.title,
    body: requestBody.body,
  };

  await createPost(newPost); // wil throw cuz auth dont work on api endpoint right now

  return new Response(null, { status: 201 });
}
