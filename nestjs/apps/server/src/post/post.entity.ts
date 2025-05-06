import { Column, Entity, ManyToOne, OneToMany, PrimaryColumn } from 'typeorm';
import { Field, Int, ObjectType } from '@nestjs/graphql';

@ObjectType()
@Entity()
export class Author {
  @Field()
  @Column()
  firstName: string;

  @Field()
  @Column()
  lastName: string;

  @Field()
  @PrimaryColumn({ type: String })
  email: string;

  @OneToMany(() => Post, (post: Post) => post.author)
  posts: Post[];
}

@ObjectType()
@Entity()
export class Post {
  @Field(() => Int)
  @PrimaryColumn()
  id: number;

  @Field()
  @Column({ type: 'text' })
  title: string;

  @Field()
  @Column({ type: 'text' })
  body: string;

  @Field(() => Author)
  @ManyToOne(() => Author, (author: Author) => author.posts, { eager: true })
  author: Author;
}
