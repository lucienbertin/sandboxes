import Link from "next/link";
import { Suspense } from "react";
import Post from "./post";
import { getPost, isInitialized } from "@/datasource";

export default async function Page({
    params,
  }: {
    params: Promise<{ id: string }>
  }) {
    await isInitialized;
    const { id: id_str } = await params;
    const id = parseInt(id_str);
    const post$ = getPost(id);

    return (
        <article className="min-h-screen flex flex-col items-center justify-center -mt-16">
            <Link href="/posts">back</Link>
            <Suspense fallback={<div>Loading...</div>}>
                <Post post$={post$}/>
            </Suspense>
        </article>
      );
  }