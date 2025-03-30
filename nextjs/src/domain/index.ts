export type Place = {
  id: number;
  name: string;
};

export type Post = {
  id: number;
  title: string;
  body: string;
  published: boolean;
};

// export enum UserRole {
//   Reader = 'reader',
//   Writer = 'writer',
//   Admin = 'admin',
// }
export type User = {
  id: number;
  firstName: string;
  lastName: string;
  email: string;
  // role: UserRole;
};
