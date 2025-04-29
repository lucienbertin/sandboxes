import { getServerSession } from "next-auth";
import SignIn from "./signin";
import Link from "next/link";
export default async function Home() {
  const session = await getServerSession();

  // const { data: session } = useSession()
  if (session?.user) {
    return (
      // <SessionProvider session={session}>
      <>
        <aside>
          <nav>
            <Link href="/posts">Posts</Link>
            <span> | </span>
            <Link href="/places">Places</Link>
          </nav>
        </aside>
        {/* Server session: Signed in as {session.user?.email} <br /> */}
        <SignIn />
      </>
      // </ SessionProvider>
    );
  }
  return (
    <>
      {/* Server session: Not signed in <br /> */}
      <SignIn />
    </>
  );
}
