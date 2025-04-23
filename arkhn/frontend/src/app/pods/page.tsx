import { getPods } from "@/backend"

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
                            <li>ip: {p.ip}, name: {p.name}, phase: {p.phase}</li>
                        ))}
                    </ol>
                </section>
            </article>
        </>
    )
}
