export type Place = {
  id: number;
  name: string;
};

export type Post = {
  id: number;
  title: string;
  body: string;
  published: boolean;
  author: User;
};

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

type AgentDelegate = () => Promise<User | null>;
type PostDelegate = (postId: number) => Promise<Post | null>;

export async function getPost(
  postId: number,
  agentDelegate:AgentDelegate,
  postDelegate: PostDelegate,
): Promise<Post> {
  const agent = await agentDelegate(); // IO - injected
  const post = await postDelegate(postId); // IO - injected

  // Domain logic
  if (!post) {
    return Promise.reject(new Error("post not found"));
  }
  if (!post.published && post.author.id != agent?.id) {
    return Promise.reject(new Error("insufficient rights"));
  }

  return post;
}

type PostsDelegate = (scope: PostScope, author: User | null) => Promise<Post[]>;
export enum PostScope {
  All, // only admins are allowed this scope
  Public, // this is for anonymous users and readers
  PublicAndFromAuthor, // this is for writers
  // i miss rust's enums, here i'd be using PublicAndFromAuthor(User) to inject the author into the enum value
}
export async function getPosts(
  agentDelegate:AgentDelegate,
  postsDelegate: PostsDelegate,
): Promise<Post[]> {
  const agent =  await agentDelegate(); // IO - injected

  // Domain logic
  let scope = PostScope.Public;
  if (agent?.role == UserRole.Admin) {
    scope = PostScope.All;
  } else if (agent?.role == UserRole.Writer) {
    scope = PostScope.PublicAndFromAuthor;
  }

  const posts = await postsDelegate(scope, agent); // IO - injected

  return posts;
}

type PostsCountDelegate = (scope: PostScope, author: User | null) => Promise<number>;
export async function countPosts(
  agentDelegate:AgentDelegate,
  postsCountDelegate: PostsCountDelegate,
): Promise<number> {
  const agent = await agentDelegate(); // IO - injected

  // Domain logic
  let scope = PostScope.Public;
  if (agent?.role == UserRole.Admin) {
    scope = PostScope.All;
  } else if (agent?.role == UserRole.Writer) {
    scope = PostScope.PublicAndFromAuthor;
  }

  const cnt = await postsCountDelegate(scope, agent); // IO - injected

  return cnt;
}

type CreatePostDelegate = (post: Partial<Post>, author: User) => Promise<void>;
export async function createPost(
  post: Partial<Post>,
  agentDelegate: AgentDelegate,
  createPostDelegate: CreatePostDelegate,
) {
  const agent = await agentDelegate(); // IO - injected
  
  // Domain logic
  if (!agent || agent.role == UserRole.Reader) {
    return Promise.reject(new Error("insufficient rights"));
  }

  await createPostDelegate(post, agent); // IO - injected
}