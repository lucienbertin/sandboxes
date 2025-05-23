import { GqlOptionsFactory } from '@nestjs/graphql';
import { Injectable } from '@nestjs/common';
import { MercuriusDriverConfig } from '@nestjs/mercurius';

@Injectable()
export class GraphqlOptions implements GqlOptionsFactory {
  createGqlOptions(): Promise<MercuriusDriverConfig> | MercuriusDriverConfig {
    return {
      autoSchemaFile: 'apps/server/src/schema.gql',
      subscription: true,
      graphiql: true,
    };
  }
}
