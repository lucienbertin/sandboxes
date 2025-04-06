"use server";
import "reflect-metadata";
import { DeepPartial, FindOptionsWhere } from "typeorm";
import { Post, PostScope, User } from "@/domain";
import { datasource, isInitialized, Post as ORMPost, User as ORMUser } from "./datasource";

function fromScope(
  scope: PostScope,
  author: User | null,
): FindOptionsWhere<Post> | FindOptionsWhere<Post>[] | undefined {
  let where = undefined;
  switch (scope) {
    case PostScope.All:
      break;
    case PostScope.PublicAndFromAuthor:
      where = [{ published: true }, { author: { id: author?.id } }];
      break;
    case PostScope.Public:
    default:
      where = { published: true };
      break;
  }

  return where;
}

export async function getPosts(
  scope: PostScope,
  author: User | null,
): Promise<Post[]> {
  await isInitialized;
  const where = fromScope(scope, author);
  const posts = await datasource.getRepository(ORMPost).find({
    where,
    relations: { author: true },
  });
  return posts.map((p) => p.asRecord());
}

export async function countPosts(
  scope: PostScope,
  author: User | null,
): Promise<number> {
  await isInitialized;
  const where = fromScope(scope, author);
  const cnt = await datasource.getRepository(ORMPost).count({
    where,
  });
  return cnt;
}

export async function getPost(id: number): Promise<Post | null> {
  await isInitialized;
  const post = await datasource.getRepository(ORMPost).findOne({
    where: { id },
    relations: { author: true },
  });

  return post?.asRecord() as Post | null;
}

export async function createPost(newPost: DeepPartial<Post>, author: User) {
  await isInitialized;
  const repo = datasource.getRepository(ORMPost);
  const post = repo.create(newPost as DeepPartial<Post>);

  const ormAuthor = await datasource
    .getRepository(ORMUser)
    .findOneByOrFail({ id: author.id });
  post.author = ormAuthor;

  await repo.save(post);
}
