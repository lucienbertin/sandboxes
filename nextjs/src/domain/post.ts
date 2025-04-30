import { ForbiddenError, NotFoundError, UnauthorizedError } from "./error";
import { AgentDelegate, AgentType, isUser, isWorker } from "./user";

export type Post = {
  id: number;
  title: string;
  body: string;
  author: string;
};

type GetPostDelegate = (postId: number) => Promise<Post | null>;
export function getPost(
  agentDelegate: AgentDelegate,
  getPostDelegate: GetPostDelegate,
): (postId: number) => Promise<Post> {
  const partial = async (postId: number) => {
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const _agent = await agentDelegate(); // IO - injected
    const post = await getPostDelegate(postId); // IO - injected

    // Domain logic
    if (!post) {
      return Promise.reject(new NotFoundError());
    } else {
      return post;
    }
  };

  return partial;
}

type GetPostsDelegate = () => Promise<Post[]>;
export function getPosts(
  agentDelegate: AgentDelegate,
  getPostsDelegate: GetPostsDelegate,
): () => Promise<Post[]> {
  const partial = async () => {
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const _agent = await agentDelegate(); // IO - injected

    const posts = await getPostsDelegate(); // IO - injected

    return posts;
  };

  return partial;
}

type CountPostsDelegate = () => Promise<number>;
export function countPosts(
  agentDelegate: AgentDelegate,
  countPostsDelegate: CountPostsDelegate,
): () => Promise<number> {
  const partial = async () => {
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const _agent = await agentDelegate(); // IO - injected

    const cnt = await countPostsDelegate(); // IO - injected

    return cnt;
  };

  return partial;
}

type UpsertPostDelegate = (post: Post) => Promise<void>;
export function upsertPost(
  agentDelegate: AgentDelegate,
  upsertPostDelegate: UpsertPostDelegate,
): (post: Post) => Promise<void> {
  const partial = async (post: Post) => {
    const agent = await agentDelegate(); // IO - injected

    // Domain logic
    if (agent == null) {
      return Promise.reject(new UnauthorizedError());
    } else if (agent._type !== AgentType.Worker) {
      return Promise.reject(new ForbiddenError());
    }

    await upsertPostDelegate(post); // IO - injected
  };

  return partial;
}

type DeletePostDelegate = (post: Post) => Promise<void>;
export function deletePost(
  agentDelegate: AgentDelegate,
  deletePostDelegate: DeletePostDelegate,
): (post: Post) => Promise<void> {
  const partial = async (post: Post) => {
    const agent = await agentDelegate(); // IO - injected

    // Domain logic
    if (agent == null) {
      return Promise.reject(new UnauthorizedError());
    } else if (!isWorker(agent)) {
      return Promise.reject(new ForbiddenError());
    }

    await deletePostDelegate(post); // IO - injected
  };

  return partial;
}

