"use client";

import { createPost, getPostsCount } from "@/datasource";
import { IPost } from "@/post.entity";
import { use, useState } from "react";
import { DeepPartial } from "typeorm";


export default function PostForm({
    cnt$,
}: {
    cnt$: Promise<number>
}) {
    const [cnt, setCnt] = useState(use(cnt$));

    const handler = async (formData: FormData) => {
        const newPost = {
            title: formData.get("title"),
            body: formData.get("body"),
        } as DeepPartial<IPost>;
        await createPost(newPost);
        setCnt(await getPostsCount());
    }

    return (
        <>
        <h1>clientside rendered form</h1>
        <form action={handler}>
            <input type="text" name="title" />
            <input type="text" name="body" />
            <button type="submit">Create</button>
        </form>
        <p>there are currently {cnt} posts in db</p>
        </>
    )
}