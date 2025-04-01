"use server";
import { User } from "@/domain";
import { datasource, isInitialized, ORMUser } from "./datasource";

export async function authenticateUser(email: string): Promise<User | null> {
  await isInitialized;

  const repo = datasource.getRepository(ORMUser);
  const user = await repo.findOneBy({ email: email });

  return user;
}

export async function getUserByEmail(email: string): Promise<User | null> {
  await isInitialized;

  const repo = datasource.getRepository(ORMUser);
  const user = await repo.findOneBy({ email: email });

  return user;
}
