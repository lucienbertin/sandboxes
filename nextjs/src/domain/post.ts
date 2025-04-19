import { ForbiddenError, NotFoundError, UnauthorizedError } from "./error";
import { AgentDelegate, User, UserRole } from "./user";

export type Post = {
  id: number;
  title: string;
  body: string;
  published: boolean;
  author: User;
};

type GetPostDelegate = (postId: number) => Promise<Post | null>;
export function getPost(
  agentDelegate: AgentDelegate,
  getPostDelegate: GetPostDelegate,
): (postId: number) => Promise<Post> {
  const partial = async (postId: number) => {
    const agent = await agentDelegate(); // IO - injected
    const post = await getPostDelegate(postId); // IO - injected

    // Domain logic
    if (!post) {
      return Promise.reject(new NotFoundError());
    }
    if (!post.published && agent === null) {
      return Promise.reject(new UnauthorizedError());
    }
    if (!post.published && agent?.role === UserRole.Admin) {
      return post;
    }
    if (!post.published && post.author.id != agent?.id) {
      return Promise.reject(new ForbiddenError());
    } else {
      return post;
    }
  };

  return partial;
}

export enum PostScope {
  All, // only admins are allowed this scope
  Public, // this is for anonymous users and readers
  PublicAndFromAuthor, // this is for writers
  // i miss rust's enums, here i'd be using PublicAndFromAuthor(User) to inject the author into the enum value
}
type GetPostsDelegate = (
  scope: PostScope,
  author: User | null,
) => Promise<Post[]>;
export function getPosts(
  agentDelegate: AgentDelegate,
  getPostsDelegate: GetPostsDelegate,
): () => Promise<Post[]> {
  const partial = async () => {
    const agent = await agentDelegate(); // IO - injected

    // Domain logic
    let scope = PostScope.Public;
    if (agent?.role == UserRole.Admin) {
      scope = PostScope.All;
    } else if (agent?.role == UserRole.Writer) {
      scope = PostScope.PublicAndFromAuthor;
    }

    const posts = await getPostsDelegate(scope, agent); // IO - injected

    return posts;
  };

  return partial;
}

type CountPostsDelegate = (
  scope: PostScope,
  author: User | null,
) => Promise<number>;
export function countPosts(
  agentDelegate: AgentDelegate,
  countPostsDelegate: CountPostsDelegate,
): () => Promise<number> {
  const partial = async () => {
    const agent = await agentDelegate(); // IO - injected

    // Domain logic
    let scope = PostScope.Public;
    if (agent?.role == UserRole.Admin) {
      scope = PostScope.All;
    } else if (agent?.role == UserRole.Writer) {
      scope = PostScope.PublicAndFromAuthor;
    }

    const cnt = await countPostsDelegate(scope, agent); // IO - injected

    return cnt;
  };

  return partial;
}

type CreatePostDelegate = (post: Partial<Post>, author: User) => Promise<void>;
export function createPost(
  agentDelegate: AgentDelegate,
  createPostDelegate: CreatePostDelegate,
): (post: Partial<Post>) => Promise<void> {
  const partial = async (post: Partial<Post>) => {
    const agent = await agentDelegate(); // IO - injected

    // Domain logic
    if (!agent) {
      return Promise.reject(new UnauthorizedError());
    } else if (agent.role == UserRole.Reader) {
      return Promise.reject(new ForbiddenError());
    }

    await createPostDelegate(post, agent); // IO - injected
  };

  return partial;
}
