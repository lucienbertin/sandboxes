import { Module } from '@nestjs/common';
import { TypeOrmModule, TypeOrmModuleOptions } from '@nestjs/typeorm';
import { GraphQLModule } from '@nestjs/graphql';
import { MercuriusDriver, MercuriusDriverConfig } from '@nestjs/mercurius';
import { GraphqlOptions } from './graphql.options';
import { MongooseModule } from '@nestjs/mongoose';
import { typeOrmConfig } from './typeorm.options';

import { PostModule } from './post';
import { CatModule } from './cat';

@Module({
  imports: [
    GraphQLModule.forRootAsync<MercuriusDriverConfig>({
      driver: MercuriusDriver,
      useClass: GraphqlOptions,
    }),
    TypeOrmModule.forRoot(typeOrmConfig as TypeOrmModuleOptions),
    MongooseModule.forRoot(process.env.MONGO_URL as string),

    PostModule, // vertical slice
    CatModule, // vertical slice
  ],
})
export class AppModule {}
