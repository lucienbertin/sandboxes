import { Args, Mutation, Query, Resolver } from '@nestjs/graphql';
import { Cat, CreateCatDto } from './cat.entity';
import { CatService } from './cat.service';

@Resolver(() => Cat)
export class CatResolver {
  constructor(private catService: CatService) {}

  @Query(() => [Cat])
  async cats() {
    return await this.catService.findAll();
  }

  @Mutation(() => Cat)
  async createCat(
    @Args('cat') cat: CreateCatDto,
  ) {
    return await this.catService.create(cat);
  }
}
