import { getServerSession } from "next-auth";
import { getUserByEmail } from "../db";
import { User } from "@/domain";

export async function resolveAgent(): Promise<User | null> {
  const session = await getServerSession();
  let agent = null;
  if (session?.user?.email) {
    agent = await getUserByEmail(session?.user?.email); // IO
  }

  return agent;
}
