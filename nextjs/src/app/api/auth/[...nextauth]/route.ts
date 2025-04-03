import { authenticateUser } from "@/infrastructure";
import NextAuth, { AuthOptions } from "next-auth";
import CredentialsProvider from "next-auth/providers/credentials";
import { Provider } from "next-auth/providers/index";
import GithubProvider from "next-auth/providers/github";
import GoogleProvider from "next-auth/providers/google";

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

const authOptions: AuthOptions = {
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
const handler = NextAuth(authOptions);

export { handler as GET, handler as POST };
