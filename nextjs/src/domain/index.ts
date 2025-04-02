import { Feature, FeatureCollection, Point } from "geojson";

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
type GetPostDelegate = (postId: number) => Promise<Post | null>;
export class NotFoundError extends Error {}
export class UnauthorizedError extends Error {}
export class ForbiddenError extends Error {}
export async function getPost(
  postId: number,
  agentDelegate: AgentDelegate,
  getPostDelegate: GetPostDelegate,
): Promise<Post> {
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
export async function getPosts(
  agentDelegate: AgentDelegate,
  getPostsDelegate: GetPostsDelegate,
): Promise<Post[]> {
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
}

type CountPostsDelegate = (
  scope: PostScope,
  author: User | null,
) => Promise<number>;
export async function countPosts(
  agentDelegate: AgentDelegate,
  countPostsDelegate: CountPostsDelegate,
): Promise<number> {
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
}

type CreatePostDelegate = (post: Partial<Post>, author: User) => Promise<void>;
export async function createPost(
  post: Partial<Post>,
  agentDelegate: AgentDelegate,
  createPostDelegate: CreatePostDelegate,
) {
  const agent = await agentDelegate(); // IO - injected

  // Domain logic
  if (!agent) {
    return Promise.reject(new UnauthorizedError());
  } else if (agent.role == UserRole.Reader) {
    return Promise.reject(new ForbiddenError());
  }

  await createPostDelegate(post, agent); // IO - injected
}

type GetPlacesGeoJSONDelegate = () => Promise<FeatureCollection<Point, Place>>;
export async function getPlacesAsGeoJSON(
  // agentDelegate: AgentDelegate,
  getPlacesDelegate: GetPlacesGeoJSONDelegate,
): Promise<FeatureCollection<Point, Place>> {
  // const agent = await agentDelegate(); // IO - injected

  const places = await getPlacesDelegate(); // IO - injected
  // should the transformation to geojson happen here or in another layer ?

  return places;
}

type CreatePlaceDelegate = (
  place: Feature<Point, Partial<Place>>,
) => Promise<void>;
export async function createPlace(
  place: Feature<Point, Partial<Place>>,
  agentDelegate: AgentDelegate,
  createPlaceDelegate: CreatePlaceDelegate,
) {
  const agent = await agentDelegate(); // IO - injected

  // Domain logic
  if (!agent) {
    return Promise.reject(new UnauthorizedError());
  } else if (agent.role == UserRole.Reader) {
    return Promise.reject(new ForbiddenError());
  }

  await createPlaceDelegate(place); // IO - injected
}
