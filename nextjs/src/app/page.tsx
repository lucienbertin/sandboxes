import { getServerSession } from "next-auth";
import SignIn from "./signin";
export default async function Home() {
  const session = await getServerSession();

  // const { data: session } = useSession()
  if (session?.user) {
    return (
      // <SessionProvider session={session}>
      <>
        Signed in as {session.user?.email} <SignIn />
      </>
      // </ SessionProvider>
    );
  }
  return (
    <>
      Not signed in <br />
      <SignIn />
    </>
  );
}
