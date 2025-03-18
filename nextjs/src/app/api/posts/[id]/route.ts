export const dynamic = 'force-static';

import { isInitialized, getPost } from "@/datasource"

export async function GET(
    _request: Request,
    { params }: { params: Promise<{ id: string }> }
  ) {
    const { id: id_str } = await params;
    const id = parseInt(id_str);
    await isInitialized;
    const post = await getPost(id);

    if (post == null) {
        return new Response(null, {
            status: 404
        });
    }

    return Response.json(post);
}