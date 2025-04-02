import {
  getPost,
  NotFoundError,
  UnauthorizedError,
  ForbiddenError,
  UserRole,
  getPosts,
  PostScope,
  countPosts,
  createPost,
} from "../";

const reader = {
  id: 1,
  firstName: "john",
  lastName: "doe",
  email: "john@d.oe",
  role: UserRole.Reader,
};
const writer = {
  id: 2,
  firstName: "kurt",
  lastName: "vonnegut",
  email: "kurt@vonneg.ut",
  role: UserRole.Writer,
};
const otherWriter = {
  id: 3,
  firstName: "enki",
  lastName: "billal",
  email: "enki@bill.al",
  role: UserRole.Writer,
};
const admin = {
  id: 4,
  firstName: "lucien",
  lastName: "bertin",
  email: "lucien@bert.in",
  role: UserRole.Admin,
};

const publishedPost = {
  id: 1,
  title: "breakfast of champions",
  body: "The expression 'breakfast of champions' is a registered trademark of General Mills, Inc., for use on a breakfast cereal product",
  published: true,
  author: writer,
};
const unpublishedPost = {
  id: 1,
  title: "draft",
  body: "work in progress",
  published: false,
  author: writer,
};

const nullDelegate = () => Promise.resolve(null);
const voidDelegate = () => Promise.resolve();

const readerDelegate = () => Promise.resolve(reader);
const writerDelegate = () => Promise.resolve(writer);
const otherWriterDelegate = () => Promise.resolve(otherWriter);
const adminDelegate = () => Promise.resolve(admin);

const publishedPostDelegate = () => Promise.resolve(publishedPost);
const unpublishedPostDelegate = () => Promise.resolve(unpublishedPost);

describe("getPost", () => {
  test("rejects with 'not found' when post delegate returns nothing", () => {
    // arrange
    const agentDelegate = nullDelegate;
    const getPostDelegate = nullDelegate;
    const postId = 1;
  
    // act
    const promise = getPost(postId, agentDelegate, getPostDelegate);
  
    // assert
    expect(promise).rejects.toBeInstanceOf(NotFoundError);
  });
  
  test("resolves with post when post delegate returns a published post", () => {
    // arrange
    const agentDelegate = nullDelegate;
    const getPostDelegate = publishedPostDelegate;
    const postId = 1;
  
    // act
    const promise = getPost(postId, agentDelegate, getPostDelegate);
  
    // assert
    expect(promise).resolves.toEqual(publishedPost);
  });
  
  test("resolves with post when post delegate returns an unpublished post and agent delegate returns its author", () => {
    // arrange
    const agentDelegate = writerDelegate;
    const getPostDelegate = unpublishedPostDelegate;
    const postId = 1;
  
    // act
    const promise = getPost(postId, agentDelegate, getPostDelegate);
  
    // assert
    expect(promise).resolves.toEqual(unpublishedPost);
  });
  
  test("rejects with 'forbidden' when post delegate returns an unpublished post and agent delegate returns another writer", () => {
    // arrange
    const agentDelegate = otherWriterDelegate;
    const getPostDelegate = unpublishedPostDelegate;
    const postId = 1;
  
    // act
    const promise = getPost(postId, agentDelegate, getPostDelegate);
  
    // assert
    expect(promise).rejects.toBeInstanceOf(ForbiddenError);
  });
  
  test("rejects with 'unauthorized' when post delegate returns an unpublished post and agent delegate returns nothing", () => {
    // arrange
    const agentDelegate = nullDelegate;
    const getPostDelegate = unpublishedPostDelegate;
    const postId = 1;
  
    // act
    const promise = getPost(postId, agentDelegate, getPostDelegate);
  
    // assert
    expect(promise).rejects.toBeInstanceOf(UnauthorizedError);
  });
  
  test("rejects with 'forbidden' when post delegate returns an unpublished post and agent delegate returns someone else than its author", () => {
    // arrange
    const agentDelegate = readerDelegate;
    const getPostDelegate = unpublishedPostDelegate;
    const postId = 1;
  
    // act
    const promise = getPost(postId, agentDelegate, getPostDelegate);
  
    // assert
    expect(promise).rejects.toBeInstanceOf(ForbiddenError);
  });
  
  test("resolves with post when post delegate returns an unpublished post and agent delegate returns an admin", () => {
    // arrange
    const agentDelegate = adminDelegate;
    const getPostDelegate = unpublishedPostDelegate;
    const postId = 1;
  
    // act
    const promise = getPost(postId, agentDelegate, getPostDelegate);
  
    // assert
    expect(promise).resolves.toEqual(unpublishedPost);
  });
  
});

describe("getPosts", () => {
  const postCollection = [publishedPost, unpublishedPost];
  const postsDelegate = () => Promise.resolve(postCollection);
  test("resolves with what getPostsDelegate returns", () => {
    // arrange
    const agentDelegate = nullDelegate;
    const getPostsDelegate = postsDelegate;
  
    // act
    const promise = getPosts(agentDelegate, getPostsDelegate);
  
    // assert
    expect(promise).resolves.toEqual(postCollection);
  });
  test("for anonymous agent, scope should be only published posts", async () => {
    // arrange
    const agentDelegate = nullDelegate;
    const mockGetPostsDelegate =  jest.fn();
    mockGetPostsDelegate.mockImplementation(postsDelegate);

    // act
    await getPosts(agentDelegate, mockGetPostsDelegate);

    // assert
    expect(mockGetPostsDelegate).toHaveBeenCalledTimes(1);
    expect(mockGetPostsDelegate).toHaveBeenCalledWith(PostScope.Public, null);
  });
  test("for reader agent, scope should be only published posts", async () => {
    // arrange
    const agentDelegate = readerDelegate;
    const mockGetPostsDelegate =  jest.fn();
    mockGetPostsDelegate.mockImplementation(postsDelegate);

    // act
    await getPosts(agentDelegate, mockGetPostsDelegate);

    // assert
    expect(mockGetPostsDelegate).toHaveBeenCalledTimes(1);
    expect(mockGetPostsDelegate).toHaveBeenCalledWith(PostScope.Public, reader);
  });
  test("for writer agent, scope should be published posts and their posts", async () => {
    // arrange
    const agentDelegate = writerDelegate;
    const mockGetPostsDelegate =  jest.fn();
    mockGetPostsDelegate.mockImplementation(postsDelegate);

    // act
    await getPosts(agentDelegate, mockGetPostsDelegate);

    // assert
    expect(mockGetPostsDelegate).toHaveBeenCalledTimes(1);
    expect(mockGetPostsDelegate).toHaveBeenCalledWith(PostScope.PublicAndFromAuthor, writer);
  });
  test("for admin agent, scope should be all posts", async () => {
    // arrange
    const agentDelegate = adminDelegate;
    const mockGetPostsDelegate =  jest.fn();
    mockGetPostsDelegate.mockImplementation(postsDelegate);

    // act
    await getPosts(agentDelegate, mockGetPostsDelegate);

    // assert
    expect(mockGetPostsDelegate).toHaveBeenCalledTimes(1);
    expect(mockGetPostsDelegate).toHaveBeenCalledWith(PostScope.All, admin);
  });
});

describe("countPosts", () => {
  const count = 2;
  const countDelegate = () => Promise.resolve(count);
  test("resolves with what countDelegate returns", () => {
    // arrange
    const agentDelegate = nullDelegate;
  
    // act
    const promise = countPosts(agentDelegate, countDelegate);
  
    // assert
    expect(promise).resolves.toEqual(count);
  });
  test("for anonymous agent, scope should be only published posts", async () => {
    // arrange
    const agentDelegate = nullDelegate;
    const mockGetCountDelegate =  jest.fn();
    mockGetCountDelegate.mockImplementation(countDelegate);

    // act
    await countPosts(agentDelegate, mockGetCountDelegate);

    // assert
    expect(mockGetCountDelegate).toHaveBeenCalledTimes(1);
    expect(mockGetCountDelegate).toHaveBeenCalledWith(PostScope.Public, null);
  });
  test("for reader agent, scope should be only published posts", async () => {
    // arrange
    const agentDelegate = readerDelegate;
    const mockGetCountDelegate =  jest.fn();
    mockGetCountDelegate.mockImplementation(countDelegate);

    // act
    await countPosts(agentDelegate, mockGetCountDelegate);

    // assert
    expect(mockGetCountDelegate).toHaveBeenCalledTimes(1);
    expect(mockGetCountDelegate).toHaveBeenCalledWith(PostScope.Public, reader);
  });
  test("for writer agent, scope should be published posts and their posts", async () => {
    // arrange
    const agentDelegate = writerDelegate;
    const mockGetCountDelegate =  jest.fn();
    mockGetCountDelegate.mockImplementation(countDelegate);

    // act
    await countPosts(agentDelegate, mockGetCountDelegate);

    // assert
    expect(mockGetCountDelegate).toHaveBeenCalledTimes(1);
    expect(mockGetCountDelegate).toHaveBeenCalledWith(PostScope.PublicAndFromAuthor, writer);
  });
  test("for admin agent, scope should be all posts", async () => {
    // arrange
    const agentDelegate = adminDelegate;
    const mockGetCountDelegate =  jest.fn();
    mockGetCountDelegate.mockImplementation(countDelegate);

    // act
    await countPosts(agentDelegate, mockGetCountDelegate);

    // assert
    expect(mockGetCountDelegate).toHaveBeenCalledTimes(1);
    expect(mockGetCountDelegate).toHaveBeenCalledWith(PostScope.All, admin);
  });
});

describe("create post", () => {
  const post = { title: "ma vie mon oeuvre", body: "je suis né un mardi matin a 8h52" };
  test("should reject with Unauthorized when agentDelegate return null", () => {
    // arrange
    const agentDelegate = nullDelegate;
    const createPostDelegate = voidDelegate;
  
    // act
    const promise = createPost(post, agentDelegate, createPostDelegate);
  
    // assert
    expect(promise).rejects.toBeInstanceOf(UnauthorizedError);
  });
  test("should reject with Forbidden when agentDelegate return a reader", () => {
    // arrange
    const agentDelegate = readerDelegate;
    const createPostDelegate = voidDelegate;
  
    // act
    const promise = createPost(post, agentDelegate, createPostDelegate);
  
    // assert
    expect(promise).rejects.toBeInstanceOf(ForbiddenError);
  });
  test("should resolve when agentDelegate returns a writer", () => {
    // arrange
    const agentDelegate = writerDelegate;
    const createPostDelegate = voidDelegate;
  
    // act
    const promise = createPost(post, agentDelegate, createPostDelegate);
  
    // assert
    expect(promise).resolves.toBe(undefined);
  });
  test("should resolve when agentDelegate returns an admin", () => {
    // arrange
    const agentDelegate = adminDelegate;
    const createPostDelegate = voidDelegate;
  
    // act
    const promise = createPost(post, agentDelegate, createPostDelegate);
  
    // assert
    expect(promise).resolves.toBe(undefined);
  });
  test("should call createPost with agent as author - writer", async () => {
    // arrange
    const agentDelegate = writerDelegate;
    const mockCreatePostDelegate =  jest.fn();
    mockCreatePostDelegate.mockImplementation(voidDelegate);
    // act
    await createPost(post, agentDelegate, mockCreatePostDelegate);
  
    // assert
    expect(mockCreatePostDelegate).toHaveBeenCalledTimes(1);
    expect(mockCreatePostDelegate).toHaveBeenCalledWith(post, writer);
  });
  test("should call createPost with agent as author - admin", async () => {
    // arrange
    const agentDelegate = adminDelegate;
    const mockCreatePostDelegate =  jest.fn();
    mockCreatePostDelegate.mockImplementation(voidDelegate);
    // act
    await createPost(post, agentDelegate, mockCreatePostDelegate);
  
    // assert
    expect(mockCreatePostDelegate).toHaveBeenCalledTimes(1);
    expect(mockCreatePostDelegate).toHaveBeenCalledWith(post, admin);
  });
});