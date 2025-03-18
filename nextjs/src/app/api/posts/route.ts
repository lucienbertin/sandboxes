export const dynamic = 'force-static';

import { isInitialized, getPublishedPosts } from "@/datasource"

export async function GET() {
    await isInitialized;
    const posts = await getPublishedPosts();

    return Response.json(posts);
}