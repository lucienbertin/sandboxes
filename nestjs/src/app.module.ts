import { Module } from '@nestjs/common';
import { TypeOrmModule, TypeOrmModuleOptions } from '@nestjs/typeorm';
import { PostModule } from './post';
import { GraphQLModule } from '@nestjs/graphql';
import { MercuriusDriver, MercuriusDriverConfig } from '@nestjs/mercurius';
import { GraphqlOptions } from './graphql.options';
import { MongooseModule } from '@nestjs/mongoose';
import { ConfigModule, ConfigService } from '@nestjs/config';
import typeormOptions from './typeorm.options';

@Module({
  imports: [
    GraphQLModule.forRootAsync<MercuriusDriverConfig>({
      driver: MercuriusDriver,
      useClass: GraphqlOptions,
    }),
    ConfigModule.forRoot({
      isGlobal: true,
      load: [typeormOptions]
    }),
    TypeOrmModule.forRootAsync({
      inject: [ConfigService],
      useFactory: async (configService: ConfigService) => (configService.get('typeorm') as Promise<TypeOrmModuleOptions>)
    }),
    MongooseModule.forRoot('mongodb://localhost/nestjs'),
    PostModule,
  ],
})
export class AppModule {}
