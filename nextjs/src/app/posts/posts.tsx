"use client";
import { use, useEffect, useState } from "react";
import Link from "next/link";
import { Post } from "@/domain";

export default function Posts({ posts$: posts$ }: { posts$: Promise<Post[]> }) {
  const posts = use(posts$);

  const [time, setTime] = useState("");

  useEffect(() => {
    setInterval(() => {
      const dateObject = new Date();

      const hour = dateObject.getHours();
      const minute = dateObject.getMinutes();
      const second = dateObject.getSeconds();

      const currentTime = hour + " : " + minute + " : " + second;

      setTime(currentTime);
    }, 1000);
  }, []);

  return (
    <section>
      <div>it is currently {time}</div>
      <ul className="font-[family-name:var(--font-geist-sans)] max-w-2xl space-y-4">
        {posts.map((post) => (
          <li key={post.id}>
            <span className="font-semibold">{post.title}</span>
            <span className="text-sm ml-2">
              by {post.author}
            </span>
            <Link className="text-sm ml-2" href={`/posts/${post.id}`}>
              {" "}
              read
            </Link>
          </li>
        ))}
      </ul>
    </section>
  );
}
