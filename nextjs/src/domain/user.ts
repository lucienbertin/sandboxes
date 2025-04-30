export enum AgentType {
  User = "user",
  Worker = "worker"
}
export enum UserRole {
  Reader = "reader",
  Writer = "writer",
  Admin = "admin",
}
export type User = {
  _type: AgentType.User,
  id: number;
  firstName: string;
  lastName: string;
  email: string;
  role: UserRole;
};

export type Worker = {
  _type: AgentType.Worker
}

export type AgentDelegate = () => Promise<User | Worker | null>;

export function isUser(agent: User | Worker | null): agent is User {
  return agent?._type === AgentType.User;
}
export function isWorker(agent: User | Worker | null): agent is Worker {
  return agent?._type === AgentType.Worker;
}