import {
  getPost,
  NotFoundError,
  UnauthorizedError,
  ForbiddenError,
  UserRole,
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

test("getPost rejects with 'not found' when post delegate returns nothing", () => {
  // arrange
  const agentDelegate = nullDelegate;
  const getPostDelegate = nullDelegate;
  const postId = 1;

  // act
  const promise = getPost(postId, agentDelegate, getPostDelegate);

  // assert
  expect(promise).rejects.toBeInstanceOf(NotFoundError);
});

test("getPost resolves with post when post delegate returns a published post", () => {
  // arrange
  const agentDelegate = nullDelegate;
  const getPostDelegate = publishedPostDelegate;
  const postId = 1;

  // act
  const promise = getPost(postId, agentDelegate, getPostDelegate);

  // assert
  expect(promise).resolves.toEqual(publishedPost);
});

test("getPost resolves with post when post delegate returns an unpublished post and agent delegate returns its author", () => {
  // arrange
  const agentDelegate = writerDelegate;
  const getPostDelegate = unpublishedPostDelegate;
  const postId = 1;

  // act
  const promise = getPost(postId, agentDelegate, getPostDelegate);

  // assert
  expect(promise).resolves.toEqual(unpublishedPost);
});

test("getPost rejects with 'forbidden' when post delegate returns an unpublished post and agent delegate returns another writer", () => {
  // arrange
  const agentDelegate = otherWriterDelegate;
  const getPostDelegate = unpublishedPostDelegate;
  const postId = 1;

  // act
  const promise = getPost(postId, agentDelegate, getPostDelegate);

  // assert
  expect(promise).rejects.toBeInstanceOf(ForbiddenError);
});

test("getPost rejects with 'unauthorized' when post delegate returns an unpublished post and agent delegate returns nothing", () => {
  // arrange
  const agentDelegate = nullDelegate;
  const getPostDelegate = unpublishedPostDelegate;
  const postId = 1;

  // act
  const promise = getPost(postId, agentDelegate, getPostDelegate);

  // assert
  expect(promise).rejects.toBeInstanceOf(UnauthorizedError);
});

test("getPost rejects with 'forbidden' when post delegate returns an unpublished post and agent delegate returns someone else than its author", () => {
  // arrange
  const agentDelegate = readerDelegate;
  const getPostDelegate = unpublishedPostDelegate;
  const postId = 1;

  // act
  const promise = getPost(postId, agentDelegate, getPostDelegate);

  // assert
  expect(promise).rejects.toBeInstanceOf(ForbiddenError);
});

test("getPost resolves with post when post delegate returns an unpublished post and agent delegate returns an admin", () => {
  // arrange
  const agentDelegate = adminDelegate;
  const getPostDelegate = unpublishedPostDelegate;
  const postId = 1;

  // act
  const promise = getPost(postId, agentDelegate, getPostDelegate);

  // assert
  expect(promise).resolves.toEqual(unpublishedPost);
});
