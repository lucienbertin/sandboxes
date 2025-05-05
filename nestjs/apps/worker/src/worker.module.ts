import { Module } from '@nestjs/common';

@Module({
  imports: [],
  controllers: [],
  providers: [],
})
export class WorkerModule {
  constructor() {
    console.log('constructing worker module');
  }
}
