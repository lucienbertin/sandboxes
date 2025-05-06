import { Args, Mutation, Query, Resolver } from '@nestjs/graphql';
import { Cat, CreateCatDto } from './cat.entity';
import { DbService } from './db.service';
import { PublisherService } from './publisher.service';

@Resolver(() => Cat)
export class CatResolver {
  constructor(
    private db: DbService,
    private publisher: PublisherService,
  ) {}

  @Query(() => [Cat])
  async cats() {
    return await this.db.findAllCats();
  }

  @Query(() => Cat)
  async cat(@Args('name', { type: () => String }) name: string) {
    return await this.db.findCatByName(name);
  }

  @Mutation(() => Cat)
  async createCat(@Args('cat') cat: CreateCatDto) {
    const createdCat = await this.db.createCat(cat);
    this.publisher.publishCatCreated(createdCat);

    return createdCat;
  }
}
