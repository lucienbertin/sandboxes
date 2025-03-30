import { authenticateUser } from "@/infrastructure"
import NextAuth, { AuthOptions } from "next-auth";
import CredentialsProvider from "next-auth/providers/credentials"
import { Provider } from "next-auth/providers/index";

const credentialsProvider: Provider = 
    CredentialsProvider({
        // The name to display on the sign in form (e.g. 'Sign in with...')
        name: 'Email',
        credentials: {
            email: { label: "Email", type: "email", placeholder: "john@d.oe" },
            password: { label: "Password", type: "password" }
        },
        async authorize(credentials, req) {
            // You need to provide your own logic here that takes the credentials
            // submitted and returns either a object representing a user or value
            // that is false/null if the credentials are invalid.
            // e.g. return { id: 1, name: 'J Smith', email: 'jsmith@example.com' }
            // You can also use the `req` object to obtain additional parameters
            // (i.e., the request IP address)
            //   const res = await fetch("/your/endpoint", {
            //     method: 'POST',
            //     body: JSON.stringify(credentials),
            //     headers: { "Content-Type": "application/json" }
            //   })
            //   const user = await res.json()
            
            //   // If no error and we have user data, return it
            //   if (res.ok && user) {
            //     return user
            //   }
            if (!credentials) {
                return null;
            } else {
                const user = await authenticateUser(credentials.email, credentials.password)
                return user as any;
            }
            // Return null if user data could not be retrieved
        }
    });


const authOptions: AuthOptions = {
    session: { strategy: "jwt" },
    providers: [credentialsProvider],
}
const handler = NextAuth(authOptions);

export { handler as GET, handler as POST };

