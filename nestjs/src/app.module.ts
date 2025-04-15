import { Module } from '@nestjs/common';
import { TypeOrmModule, TypeOrmModuleOptions } from '@nestjs/typeorm';
import { PostModule } from './post';
import { GraphQLModule } from '@nestjs/graphql';
import { MercuriusDriver, MercuriusDriverConfig } from '@nestjs/mercurius';
import { GraphqlOptions } from './graphql.options';
import { MongooseModule } from '@nestjs/mongoose';
import { typeOrmConfig } from './typeorm.options';

@Module({
  imports: [
    GraphQLModule.forRootAsync<MercuriusDriverConfig>({
      driver: MercuriusDriver,
      useClass: GraphqlOptions,
    }),
    TypeOrmModule.forRoot(
      typeOrmConfig as TypeOrmModuleOptions
    ),
    MongooseModule.forRoot('mongodb://localhost/nestjs'),
    PostModule,
  ],
})
export class AppModule {}
