import prisma from "@/lib/prisma";
import Link from "next/link";
import { Suspense } from "react";
import Post from "./post";

export default async function Page({
    params,
  }: {
    params: Promise<{ id: string }>
  }) {
    const { id: id_str } = await params;
    const id = parseInt(id_str);

    const post$ = prisma.post.findUnique({
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
            <Suspense fallback={<div>Loading...</div>}>
                <Post post$={post$}/>
            </Suspense>
        </article>
      );
  }