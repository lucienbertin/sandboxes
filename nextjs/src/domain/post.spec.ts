import { getPost, getPosts, countPosts, deletePost, upsertPost } from "./post";
import { ForbiddenError, NotFoundError, UnauthorizedError } from "./error";
import { AgentType, User, UserRole, Worker } from "./user";

const user = {
  _type: AgentType.User,
  id: 1,
  firstName: "john",
  lastName: "doe",
  email: "john@d.oe",
  role: UserRole.Reader,
} as User;
const admin = {
  _type: AgentType.User,
  id: 2,
  firstName: "al",
  lastName: "mighty",
  email: "al@migh.ty",
  role: UserRole.Admin,
} as User;
const worker = { _type: AgentType.Worker } as Worker;

const post = {
  id: 1,
  title: "breakfast of champions",
  body: "The expression 'breakfast of champions' is a registered trademark of General Mills, Inc., for use on a breakfast cereal product",
  author: "john doe",
};

const nullDelegate = () => Promise.resolve(null);
const voidDelegate = () => Promise.resolve();

const userDelegate = () => Promise.resolve(user);
const adminDelegate = () => Promise.resolve(admin);
const workerDelegate = () => Promise.resolve(worker);

const postDelegate = () => Promise.resolve(post);

describe("getPost", () => {
  describe("as anonymous agent", () => {
    test("rejects with 'not found' when post delegate returns nothing", () => {
      // arrange
      const agentDelegate = nullDelegate;
      const getPostDelegate = nullDelegate;
      const postId = 1;

      // act
      const promise = getPost(agentDelegate, getPostDelegate)(postId);

      // assert
      expect(promise).rejects.toBeInstanceOf(NotFoundError);
    });

    test("resolves with post when post delegate returns a published post", () => {
      // arrange
      const agentDelegate = nullDelegate;
      const getPostDelegate = postDelegate;
      const postId = 1;

      // act
      const promise = getPost(agentDelegate, getPostDelegate)(postId);

      // assert
      expect(promise).resolves.toEqual(post);
    });
  });

  describe("as user agent", () => {
    test("rejects with 'not found' when post delegate returns nothing", () => {
      // arrange
      const agentDelegate = userDelegate;
      const getPostDelegate = nullDelegate;
      const postId = 1;

      // act
      const promise = getPost(agentDelegate, getPostDelegate)(postId);

      // assert
      expect(promise).rejects.toBeInstanceOf(NotFoundError);
    });

    test("resolves with post when post delegate returns a published post", () => {
      // arrange
      const agentDelegate = userDelegate;
      const getPostDelegate = postDelegate;
      const postId = 1;

      // act
      const promise = getPost(agentDelegate, getPostDelegate)(postId);

      // assert
      expect(promise).resolves.toEqual(post);
    });
  });
  describe("as worker agent", () => {
    test("rejects with 'not found' when post delegate returns nothing", () => {
      // arrange
      const agentDelegate = workerDelegate;
      const getPostDelegate = nullDelegate;
      const postId = 1;

      // act
      const promise = getPost(agentDelegate, getPostDelegate)(postId);

      // assert
      expect(promise).rejects.toBeInstanceOf(NotFoundError);
    });

    test("resolves with post when post delegate returns a published post", () => {
      // arrange
      const agentDelegate = workerDelegate;
      const getPostDelegate = postDelegate;
      const postId = 1;

      // act
      const promise = getPost(agentDelegate, getPostDelegate)(postId);

      // assert
      expect(promise).resolves.toEqual(post);
    });
  });
});

describe("getPosts", () => {
  const postCollection = [post];
  const postsDelegate = () => Promise.resolve(postCollection);
  
  describe("as anonymous agent", () => {
    test("resolves with what getPostsDelegate returns", () => {
      // arrange
      const agentDelegate = nullDelegate;
      const getPostsDelegate = postsDelegate;

      // act
      const promise = getPosts(agentDelegate, getPostsDelegate)();

      // assert
      expect(promise).resolves.toEqual(postCollection);
    });
  });
  
  describe("as user agent", () => {
    test("resolves with what getPostsDelegate returns", () => {
      // arrange
      const agentDelegate = userDelegate;
      const getPostsDelegate = postsDelegate;

      // act
      const promise = getPosts(agentDelegate, getPostsDelegate)();

      // assert
      expect(promise).resolves.toEqual(postCollection);
    });
  });
  
  describe("as worker agent", () => {
    test("resolves with what getPostsDelegate returns", () => {
      // arrange
      const agentDelegate = nullDelegate;
      const getPostsDelegate = postsDelegate;

      // act
      const promise = getPosts(agentDelegate, getPostsDelegate)();

      // assert
      expect(promise).resolves.toEqual(postCollection);
    });
  });
});

describe("countPosts", () => {
  const count = 2;
  const countDelegate = () => Promise.resolve(count);
  describe("as anonymous agent", () => {
    test("resolves with what countDelegate returns", () => {
      // arrange
      const agentDelegate = nullDelegate;

      // act
      const promise = countPosts(agentDelegate, countDelegate)();

      // assert
      expect(promise).resolves.toEqual(count);
    });
  });
  describe("as user agent", () => {
    test("resolves with what countDelegate returns", () => {
      // arrange
      const agentDelegate = userDelegate;

      // act
      const promise = countPosts(agentDelegate, countDelegate)();

      // assert
      expect(promise).resolves.toEqual(count);
    });
  });
  describe("as worker agent", () => {
    test("resolves with what countDelegate returns", () => {
      // arrange
      const agentDelegate = workerDelegate;

      // act
      const promise = countPosts(agentDelegate, countDelegate)();

      // assert
      expect(promise).resolves.toEqual(count);
    });
  });
});

describe("upsertPost", () => {
  const post = {
    id: 12,
    title: "ma vie mon oeuvre",
    body: "je suis né un mardi matin a 8h52",
    author: "jean michel imbu de soi meme"
  };
  describe("as anonymous agent", () => {
    test("should reject with Unauthorized error", () => {
      // arrange
      const agentDelegate = nullDelegate;
      const upserPostDelegate = voidDelegate;
  
      // act
      const promise = upsertPost(agentDelegate, upserPostDelegate)(post);
  
      // assert
      expect(promise).rejects.toBeInstanceOf(UnauthorizedError);
    });
  });
  describe("as user agent", () => {
    test("should reject with Forbidden error", () => {
      // arrange
      const agentDelegate = userDelegate;
      const upserPostDelegate = voidDelegate;
  
      // act
      const promise = upsertPost(agentDelegate, upserPostDelegate)(post);
  
      // assert
      expect(promise).rejects.toBeInstanceOf(ForbiddenError);
    });
    test("should reject with Forbidden error even for an admin", () => {
      // arrange
      const agentDelegate = adminDelegate;
      const upserPostDelegate = voidDelegate;
  
      // act
      const promise = upsertPost(agentDelegate, upserPostDelegate)(post);
  
      // assert
      expect(promise).rejects.toBeInstanceOf(ForbiddenError);
    });
  });
  describe("as worker agent", () => {
    test("should resolve when agentDelegate returns a writer", () => {
      // arrange
      const agentDelegate = workerDelegate;
      const upserPostDelegate = voidDelegate;
  
      // act
      const promise = upsertPost(agentDelegate, upserPostDelegate)(post);
  
      // assert
      expect(promise).resolves.toBe(undefined);
    });
    test("should call upsert post delegate with post", async () => {
      // arrange
      const agentDelegate = workerDelegate;
      const upserPostDelegate = jest.fn();
      upserPostDelegate.mockImplementation(voidDelegate);

      // act
      await upsertPost(agentDelegate, upserPostDelegate)(post);
  
      // assert
      expect(upserPostDelegate).toHaveBeenCalledTimes(1);
      expect(upserPostDelegate).toHaveBeenCalledWith(post);
    });
  });
});

describe("deletePost", () => {
  const post = {
    id: 12,
    title: "ma vie mon oeuvre",
    body: "je suis né un mardi matin a 8h52",
    author: "jean michel imbu de soi meme"
  };
  describe("as anonymous agent", () => {
    test("should reject with Unauthorized error", () => {
      // arrange
      const agentDelegate = nullDelegate;
      const deletePostDelegate = voidDelegate;
  
      // act
      const promise = deletePost(agentDelegate, deletePostDelegate)(post);
  
      // assert
      expect(promise).rejects.toBeInstanceOf(UnauthorizedError);
    });
  });
  describe("as user agent", () => {
    test("should reject with Forbidden error", () => {
      // arrange
      const agentDelegate = userDelegate;
      const deletePostDelegate = voidDelegate;
  
      // act
      const promise = deletePost(agentDelegate, deletePostDelegate)(post);
  
      // assert
      expect(promise).rejects.toBeInstanceOf(ForbiddenError);
    });
    test("should reject with Forbidden error even for an admin", () => {
      // arrange
      const agentDelegate = adminDelegate;
      const deletePostDelegate = voidDelegate;
  
      // act
      const promise = deletePost(agentDelegate, deletePostDelegate)(post);
  
      // assert
      expect(promise).rejects.toBeInstanceOf(ForbiddenError);
    });
  });
  describe("as worker agent", () => {
    test("should resolve when agentDelegate returns a writer", () => {
      // arrange
      const agentDelegate = workerDelegate;
      const deletePostDelegate = voidDelegate;
  
      // act
      const promise = deletePost(agentDelegate, deletePostDelegate)(post);
  
      // assert
      expect(promise).resolves.toBe(undefined);
    });
    test("should call upsert post delegate with post", async () => {
      // arrange
      const agentDelegate = workerDelegate;
      const deletePostDelegate = jest.fn();
      deletePostDelegate.mockImplementation(voidDelegate);

      // act
      await deletePost(agentDelegate, deletePostDelegate)(post);
  
      // assert
      expect(deletePostDelegate).toHaveBeenCalledTimes(1);
      expect(deletePostDelegate).toHaveBeenCalledWith(post);
    });
  });
});
