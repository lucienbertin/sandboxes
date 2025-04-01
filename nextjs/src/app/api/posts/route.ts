export const dynamic = "force-static";

import { getPublishedPosts, createPost } from "@/infrastructure";
import { Post, UserRole } from "@/domain";
import { NextRequest } from "next/server";

export async function GET() {
  const posts = await getPublishedPosts();

  return Response.json(posts);
}

export async function POST(request: NextRequest) {
  const requestBody = await request.json();
  const newPost: Partial<Post> = {
    title: requestBody.title,
    body: requestBody.body,
  };

  // HARD CODE
  const johnDoe = {
    id: 1,
    firstName: "john",
    lastName: "doe",
    email: "john@d.oe",
    role: UserRole.Writer,
  };

  await createPost(newPost, johnDoe);

  return new Response(null, { status: 201 });
}
