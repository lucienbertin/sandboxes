"use server"

import { getPodLogs } from "@/backend";
import Link from "next/link";
export default async function Page({
    params,
  }: {
    params: Promise<{ name: string }>;
  }) {
    const { name: name } = await params;
    const logs = await getPodLogs(name);



    return (
        <>
            <article>
                <header>
                    <h1>Pod {name}</h1>
                    <nav>
                        <Link href="/pods">&lt; back</Link>
                    </nav>
                </header>
                <section>
                    <h2>logs</h2>
                    <pre>
                        {logs}
                    </pre>
                </section>
            </article>
        </>
    )
}
