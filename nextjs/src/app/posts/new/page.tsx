import { createPost, getPostsCount } from "@/actions";
import Link from "next/link";
import PostForm from "./form";
import { Suspense } from "react";
import { Post } from "@/domain";
import { revalidatePath } from "next/cache";

export default async function Page() {
  const cnt$ = getPostsCount();
  const cnt = await getPostsCount();
  const handler = async (formData: FormData) => {
    "use server";
    const newPost = {
      title: formData.get("title"),
      body: formData.get("body"),
    } as Partial<Post>;
    await createPost(newPost);

    revalidatePath("/posts/new");
  };
  return (
    <>
      <article className="min-h-screen flex flex-col items-center justify-center -mt-16">
        <Link href="/posts">back</Link>
        <h1>serverside rendered form</h1>
        <form action={handler}>
          <input type="text" name="title" />
          <input type="text" name="body" />
          <button type="submit">Create</button>
        </form>
        <p>i can consult {cnt} posts in db</p>
        <Suspense fallback={<div>Loading...</div>}>
          <PostForm cnt$={cnt$} />
        </Suspense>
      </article>
    </>
  );
}
