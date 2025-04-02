import {
  getPost,
  NotFoundError,
  UnauthorizedError,
  ForbiddenError,
  UserRole,
  getPosts,
  PostScope,
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
  const postsDelegate = () => Promise.resolve([publishedPost, unpublishedPost]);
  test("resolves with what getPostsDelegate returns", () => {
    // arrange
    const agentDelegate = nullDelegate;
    const getPostsDelegate = postsDelegate;
  
    // act
    const promise = getPosts(agentDelegate, getPostsDelegate);
  
    // assert
    expect(promise).resolves.toEqual([publishedPost, unpublishedPost]);
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