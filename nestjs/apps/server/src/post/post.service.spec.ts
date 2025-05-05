import { Test, TestingModule } from '@nestjs/testing';
import { PostService } from './post.service';
import { Repository } from 'typeorm';
import { Post } from './post.entity';

class MockPostRepo extends Repository<Post> {
  // eslint-disable-next-line @typescript-eslint/require-await
  public async find(): Promise<Post[]> {
    return [];
  }
}

describe('PostService', () => {
  let service: PostService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        {
          provide: 'PostRepository',
          useClass: MockPostRepo,
        },
        PostService,
      ],
    }).compile();

    service = module.get<PostService>(PostService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
