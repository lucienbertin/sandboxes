import { authenticateUser } from "@/infrastructure";
import NextAuth, { AuthOptions } from "next-auth";
import CredentialsProvider from "next-auth/providers/credentials";
import { Provider } from "next-auth/providers/index";

const credentialsProvider: Provider = CredentialsProvider({
  // The name to display on the sign in form (e.g. 'Sign in with...')
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
  providers: [credentialsProvider],
};
const handler = NextAuth(authOptions);

export { handler as GET, handler as POST };
