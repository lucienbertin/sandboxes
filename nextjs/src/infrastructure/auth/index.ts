import { getServerSession } from "next-auth";
import { authenticateUser, getUserByEmail } from "../db";
import { User } from "@/domain";
import { AuthOptions } from "next-auth";
import { Provider } from "next-auth/providers/index";
import CredentialsProvider from "next-auth/providers/credentials";
import GithubProvider from "next-auth/providers/github";
import GoogleProvider from "next-auth/providers/google";

export async function resolveAgent(): Promise<User | null> {
  const session = await getServerSession();
  let agent = null;
  if (session?.user?.email) {
    agent = await getUserByEmail(session?.user?.email); // IO
  }

  return agent;
}

const credentialsProvider: Provider = CredentialsProvider({
  name: "Email",
  credentials: {
    email: { label: "Email", type: "email", placeholder: "john@d.oe" },
    password: { label: "Password", type: "password" },
  },
  async authorize(credentials) {
    if (!credentials) {
      return null;
    } else {
      const user = await authenticateUser(credentials.email);
      return user as any; // eslint-disable-line @typescript-eslint/no-explicit-any
    }
  },
});

export const authOptions: AuthOptions = {
  session: { strategy: "jwt" },
  providers: [
    credentialsProvider,
    GoogleProvider({
      clientId: process.env.GOOGLE_ID as string,
      clientSecret: process.env.GOOGLE_SECRET as string,
    }),
    GithubProvider({
      clientId: process.env.GITHUB_ID as string,
      clientSecret: process.env.GITHUB_SECRET as string,
    }),
  ],
};
