import { Test, TestingModule } from '@nestjs/testing';
import { WorkerController } from './worker.controller';
import { WorkerService } from './worker.service';
import { Post } from './models';

class MockService {
  // eslint-disable-next-line @typescript-eslint/require-await
  async upsertPost(post: Post) {
    return Promise.resolve();
  }
  async deletePost(post: Post) {
    return Promise.resolve();
  }

}
describe('WorkerController', () => {
  let controller: WorkerController;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      controllers: [WorkerController],
      providers: [
        {
          provide: WorkerService,
          useClass: MockService,
        },
      ]
    }).compile();

    controller = module.get<WorkerController>(WorkerController);
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
  });
});
