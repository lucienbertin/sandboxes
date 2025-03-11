'use client'
import { Post as PostModel, User } from '@prisma/client'
import { use } from 'react'
 
export default function Post({
  post$,
}: {
  post$: Promise<(PostModel & { author: User }) | null>
}) {
  const post = use(post$);
 
  return (
    <article className="min-h-screen flex flex-col items-center justify-center -mt-16">
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