"use server"
import { deleteDeployment, getDeployment } from "@/actions";
import Link from "next/link";
import { redirect } from "next/navigation";
export default async function Page({
    params,
  }: {
    params: Promise<{ name: string }>;
  }) {
    const { name: name } = await params;
    const deployment = await getDeployment(name);

    async function handler() {
        "use server"
        await deleteDeployment(deployment.name);
        redirect("/deployments");
    }

    return (
        <>
            <article>
                <header>
                    <h1>Deployment {deployment.name}</h1>
                    <nav>
                        <Link href="/deployments">&lt; back</Link>
                    </nav>
                </header>
                <section>
                <dl>
                    <dt>Image</dt>
                    <dd>{deployment.image}</dd>

                    <dt>Replicas</dt>
                    <dd>{deployment.replicas}</dd>
                </dl>
                    <form action={handler}>
                        <button type="submit">Delete</button>
                    </form>
                </section>
            </article>
        </>
    )
}
