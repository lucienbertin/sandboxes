import prisma from "@/lib/prisma";
import Link from "next/link";

export default async function Posts() {
  const posts = await prisma.post.findMany({
    include: {
      author: true,
    },
    where: {
        published: true,
    }
  });

  return (
    <div className="min-h-screen flex flex-col items-center justify-center -mt-16">
      <h1 className="text-4xl font-bold mb-8 font-[family-name:var(--font-geist-sans)]">
        Posts
      </h1>
      <ul className="font-[family-name:var(--font-geist-sans)] max-w-2xl space-y-4">
        {posts.map((post) => (
          <li key={post.id}>
            <span className="font-semibold">{post.title}</span>
            <span className="text-sm ml-2">
              by {post.author.firstName} {post.author.lastName} 
            </span>
            <Link className="text-sm ml-2" href={`/posts/${post.id}`}> read</Link>
          </li>
        ))}
      </ul>
    </div>
  );
}