"use client";

import { useSession, signIn, signOut } from "next-auth/react";
// import { credentialsProvider } from "./api/auth/[...nextauth]/route"

export default function SignIn() {
  const { data: session } = useSession();
  console.log(session);
  if (session?.user) {
    return (
      <>
        Client session: Signed in as {session.user.email} <br />
        <button onClick={() => signOut()}>Sign out</button>
      </>
    );
  }
  return (
    <>
      Client session: Not signed in <br />
      <button onClick={() => signIn()}>Sign in</button>
    </>
  );
}
