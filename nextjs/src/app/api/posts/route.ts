export const dynamic = "force-static";

import { getPosts} from "@/actions";

export async function GET() {
  const posts = await getPosts();

  return Response.json(posts);
}
