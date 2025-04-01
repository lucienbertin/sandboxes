"use client";

import { createPost, getPostsCount } from "@/actions";
import { Post } from "@/domain";
import { use, useState } from "react";

export default function PostForm({ cnt$ }: { cnt$: Promise<number> }) {
  const [cnt, setCnt] = useState(use(cnt$));

  const handler = async (formData: FormData) => {
    const newPost = {
      title: formData.get("title"),
      body: formData.get("body"),
    } as Partial<Post>;
    await createPost(newPost);
    setCnt(await getPostsCount());
  };

  return (
    <>
      <h1>clientside rendered form</h1>
      <form action={handler}>
        <input type="text" name="title" />
        <input type="text" name="body" />
        <button type="submit">Create</button>
      </form>
      <p>i can consult {cnt} posts in db</p>
    </>
  );
}
