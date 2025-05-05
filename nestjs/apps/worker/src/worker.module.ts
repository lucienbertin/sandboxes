import { Module } from '@nestjs/common';
// import { WorkerService } from './worker.service';
import { WorkerController } from './worker.controller';

@Module({
  imports: [],
  // providers: [WorkerService],
  controllers: [WorkerController],
})
export class WorkerModule {
  constructor() {}
}
