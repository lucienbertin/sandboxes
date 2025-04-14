import { Module } from '@nestjs/common';
import { PostService } from './post.service';
import { TypeOrmModule } from '@nestjs/typeorm';
import { Post } from './post.entity';
import { PostResolver } from './post.resolver';
import { CatService } from './cat.service';
import { CatResolver } from './cat.resolver';
import { MongooseModule } from '@nestjs/mongoose';
import { Cat, CatSchema } from './cat.entity';

@Module({
  imports: [
    TypeOrmModule.forFeature([Post]),
    MongooseModule.forFeature([{ name: Cat.name, schema: CatSchema }]),
  ],
  providers: [CatService, CatResolver, PostService, PostResolver],
})
export class PostModule {}
