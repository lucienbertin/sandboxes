import { getDeployments } from "@/web";
import Link from "next/link";

export default async function Page() {
    const deployments = await getDeployments();
    return (
        <>
            <article>
                <header>
                    <h1>Deployments</h1>
                    <Link href="/deployments/create">+ Create</Link>
                </header>
                <section>
                    <ol>
                        {deployments.map(d => (
                            <li key={d.name} >
                                <span>name: {d.name} </span>
                                <Link href={`/deployments/${d.name}`}>consult</Link>
                            </li>
                        ))}
                    </ol>
                </section>
            </article>
        </>
    )
}
