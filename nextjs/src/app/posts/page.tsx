import { Suspense } from "react";
import Posts from "./posts";

import { getPublishedPosts } from "@/infrastructure";
import { Post } from "@/domain";

export default async function Page() {
  const posts$: Promise<Post[]> = getPublishedPosts();

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