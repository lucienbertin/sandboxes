export const dynamic = "force-static";

import { getPost } from "@/actions";

export async function GET(
  _request: Request,
  { params }: { params: Promise<{ id: string }> },
) {
  const { id: id_str } = await params;
  const id = parseInt(id_str);
  try {
    const post = await getPost(id);
    return Response.json(post);
  } catch (e) { // eslint-disable-line @typescript-eslint/no-unused-vars
    return new Response(null, {
      status: 404,
    });
  }
}
