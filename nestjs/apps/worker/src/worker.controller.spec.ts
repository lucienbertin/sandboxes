import { Test, TestingModule } from '@nestjs/testing';
import { WorkerController } from './worker.controller';
import { WorkerService } from './worker.service';

class MockService {
  async upsertPost() {
    return Promise.resolve();
  }
  async deletePost() {
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
      ],
    }).compile();

    controller = module.get<WorkerController>(WorkerController);
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
  });
});
