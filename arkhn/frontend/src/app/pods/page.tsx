import { getPods } from "@/backend"
import Link from "next/link";

export default async function Page() {
    const pods = await getPods();
    return (
        <>
            <article>
                <header>
                    <h1>Pods</h1>
                </header>
                <section>
                    <ol>
                        { pods.map(p => (
                            <li key={p.name}>
                                <span>ip: {p.ip}, name: {p.name}, phase: {p.phase} </span>
                                <Link href={`/pods/${p.name}`}>consult</Link>
                            </li>
                        ))}
                    </ol>
                </section>
            </article>
        </>
    )
}
