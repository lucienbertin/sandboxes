import prisma from "@/lib/prisma";
import Link from "next/link";

export default async function Page({
    params,
  }: {
    params: Promise<{ id: string }>
  }) {
    const { id: id_str } = await params;
    const id = parseInt(id_str);

    const post = await prisma.post.findUnique({
        where: {
            id: id,
        },
        include: {
          author: true,
        },
      });
    return (
        <article className="min-h-screen flex flex-col items-center justify-center -mt-16">
        <Link href="/posts">back</Link>
          <h1 className="text-4xl font-bold mb-8 font-[family-name:var(--font-geist-sans)]">
            {post?.title}
            <span className="text-sm ml-2">
              by {post?.author.firstName} {post?.author.lastName} 
            </span>
          </h1>
          <p>{post?.body}</p>
        </article>
      );
  }