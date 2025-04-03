import { getServerSession } from "next-auth";
import SignIn from "./signin";
export default async function Home() {
  const session = await getServerSession();

  // const { data: session } = useSession()
  if (session?.user) {
    return (
      // <SessionProvider session={session}>
      <>
        Server session: Signed in as {session.user?.email} <br /><SignIn />
      </>
      // </ SessionProvider>
    );
  }
  return (
    <>
      Server session:  Not signed in <br />
      <SignIn />
    </>
  );
}
