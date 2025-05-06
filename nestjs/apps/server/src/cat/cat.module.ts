import { Module } from '@nestjs/common';
import { DbService } from './db.service';
import { CatResolver } from './cat.resolver';
import { MongooseModule } from '@nestjs/mongoose';
import { Cat, CatSchema } from './cat.entity';
import { PublisherService } from './publisher.service';

@Module({
  imports: [MongooseModule.forFeature([{ name: Cat.name, schema: CatSchema }])],
  providers: [
    DbService,
    CatResolver,
    {
      provide: PublisherService,
      useFactory: async () => {
        const publisher = new PublisherService();
        await publisher.init();
        return publisher;
      },
    },
  ],
})
export class CatModule {}
