import { Module } from '@nestjs/common';
import { TypeOrmModule } from '@nestjs/typeorm';
import { PostModule } from './post';
import { GraphQLModule } from '@nestjs/graphql';
import { MercuriusDriver, MercuriusDriverConfig } from '@nestjs/mercurius';
import { GraphqlOptions } from './graphql.options';
import { MongooseModule } from '@nestjs/mongoose';

@Module({
  imports: [
    GraphQLModule.forRootAsync<MercuriusDriverConfig>({
      driver: MercuriusDriver,
      useClass: GraphqlOptions,
    }),
    TypeOrmModule.forRoot({
      type: 'postgres',
      host: 'localhost',
      port: 5432,
      password: 'nestjs',
      username: 'nestjs',
      autoLoadEntities: true,
      database: 'nestjs',
      synchronize: true,
      logging: true,
    }),
    MongooseModule.forRoot('mongodb://localhost/nestjs'),
    PostModule,
  ],
})
export class AppModule {}
