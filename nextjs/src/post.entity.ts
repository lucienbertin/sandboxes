import { Column, Entity, PrimaryGeneratedColumn } from 'typeorm';

export interface IPost {
  id: number;
  title: string;
  body: string;
  published: boolean;
}

@Entity({ name: 'posts' })
export class Post implements IPost {
  @PrimaryGeneratedColumn()
  id!: number;

  @Column({ type: 'text' })
  title!: string;

  @Column({ type: 'text' })
  body!: string;

  @Column({ type: 'boolean' })
  published!: boolean;

  asStruct(): IPost {
    return { ...this } as IPost;
  }
}
