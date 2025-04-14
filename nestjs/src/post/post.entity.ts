import { Column, Entity, PrimaryGeneratedColumn } from 'typeorm';
import { Field, Int, ObjectType } from '@nestjs/graphql';

@ObjectType()
@Entity({ name: 'posts' })
export class Post {
  @Field(type => Int)
  @PrimaryGeneratedColumn()
  id: number;

  @Field({ nullable: true })
  @Column({ type: 'text' })
  title: string;

  @Field({ nullable: true })
  @Column({ type: 'text' })
  body: string;

  @Field(type => Boolean)
  @Column({ type: 'boolean' })
  published: boolean;
}
