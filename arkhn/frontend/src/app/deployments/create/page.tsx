import { createDeployment } from "@/actions";
import Link from "next/link";
import { redirect } from "next/navigation";

export default function Page() {
    async function handler(formData) {
        "use server"
        await createDeployment(formData);
        redirect("/deployments");
    }
    return (
        <>
            <article>
                <header>
                    <h1>Create deployment</h1>
                    <nav>
                        <Link href="/deployments">&lt; back</Link>
                    </nav>
                </header>
                <section>
                    <form action={handler}>
                        <label>
                            Name
                            <input type="text" name="name" />
                        </label> <br/>
                        <label>
                            Image
                            <input type="text" name="image" />
                        </label> <br/>
                        <label>
                            Replicas
                            <input type="number" name="replicas" />
                        </label> <br/>
                        <button type="submit">Create</button>
                    </form>
                </section>
            </article>
        </>
    )

}