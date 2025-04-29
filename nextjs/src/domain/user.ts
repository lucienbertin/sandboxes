export enum UserRole {
  Reader = "reader",
  Writer = "writer",
  Admin = "admin",
}
export type User = {
  id: number;
  firstName: string;
  lastName: string;
  email: string;
  role: UserRole;
};

export type AgentDelegate = () => Promise<User | null>;
