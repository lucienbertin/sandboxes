import { Suspense } from "react";
import Posts from "./posts";

import { isInitialized, getPublishedPosts } from "@/datasource";
import { IPost, Post } from "@/post.entity";

export default async function Page() {


  await isInitialized;
  const posts$: Promise<IPost[]> = getPublishedPosts();

  return (
    <div className="min-h-screen flex flex-col items-center justify-center -mt-16">
      <h1 className="text-4xl font-bold mb-8 font-[family-name:var(--font-geist-sans)]">
        Posts
      </h1>
      <Suspense fallback={<div>Loading...</div>}>
        <Posts posts$={posts$} />
      </Suspense>
    </div>
  );
}