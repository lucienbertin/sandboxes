import { Module } from '@nestjs/common';
import { WorkerController } from './worker.controller';
import { Pool } from 'pg';
import { WorkerService } from './worker.service';

@Module({
  providers: [
    {
      provide: 'PG_CONNECTION',
      useValue: new Pool({
        host: process.env.DB_HOST,
        database: 'nestjs-db',
        user: 'nestjs',
        password: 'nestjs',
        port: 5432,
      }),
    },
    WorkerService,
  ],
  controllers: [WorkerController],
})
export class WorkerModule {
  constructor() {}
}
