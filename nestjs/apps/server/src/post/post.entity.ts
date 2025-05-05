import { Column, Entity, ManyToOne, OneToMany, PrimaryColumn } from 'typeorm';
import { Field, Int, ObjectType } from '@nestjs/graphql';

@Entity()
export class Author {
  @Column()
  firstName: string;

  @Column()
  lastName: string;

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

  // @Field()
  @ManyToOne(() => Author, (author: Author) => author.posts)
  author: Author;
}
