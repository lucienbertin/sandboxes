import { Injectable } from '@nestjs/common';
import { InjectRepository } from '@nestjs/typeorm';
import { Repository } from 'typeorm';
import { Post } from './post.entity';

@Injectable()
export class PostService {
  constructor(
    @InjectRepository(Post) private readonly repo: Repository<Post>,
  ) {}

  findAll(): Promise<Post[]> {
    return this.repo.find({
      relations: {
        author: true,
      },
    });
  }

  findOneById(id: number): Promise<Post | null> {
    return this.repo.findOneBy({ id: id });
  }
}
