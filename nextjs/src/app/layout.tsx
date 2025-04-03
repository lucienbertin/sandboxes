import type { Metadata } from "next";
import "./globals.css";
import { getServerSession } from "next-auth";
import { ClientSessionProvider } from "./client-session-provider";

export const metadata: Metadata = {
  title: "Sandoxes - NextJs",
  description: "web app sandbox using nextjs framework",
};

export default async function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  const session = await getServerSession();
  return (
    <html lang="en">
      <body>
        <ClientSessionProvider session={session}>
          {children}
        </ClientSessionProvider>
      </body>
    </html>
  );
}
