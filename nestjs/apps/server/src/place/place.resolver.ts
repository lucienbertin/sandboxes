import { Args, Int, Query, Resolver } from '@nestjs/graphql';
import { PlaceService } from './place.service';
import { Place } from './place.entity';

@Resolver()
export class PlaceResolver {
  constructor(private readonly service: PlaceService) {}

  @Query(() => [Place])
  async places() {
    return this.service.findAll();
  }

  @Query(() => Place)
  async place(@Args('id', { type: () => Int }) id: number) {
    return this.service.findOneById(id);
  }
}
