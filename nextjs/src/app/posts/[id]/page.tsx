import Link from "next/link";
import { Suspense } from "react";
import PostComponent from "./post";
import { getPost } from "@/actions";
import { redirect } from "next/navigation";

export default async function Page({
  params,
}: {
  params: Promise<{ id: string }>;
}) {
  const { id: id_str } = await params;
  const id = parseInt(id_str);
  const post$ = getPost(id).catch(() => redirect("/"));

  return (
    <article className="min-h-screen flex flex-col items-center justify-center -mt-16">
      <Link href="/posts">back</Link>
      <Suspense fallback={<div>Loading...</div>}>
        <PostComponent post$={post$} />
      </Suspense>
    </article>
  );
}
