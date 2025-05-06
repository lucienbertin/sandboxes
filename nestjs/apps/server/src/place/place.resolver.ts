import { Resolver } from '@nestjs/graphql';
import { PlaceService } from './place.service';

@Resolver()
export class PlaceResolver {
  constructor(private readonly placeService: PlaceService) {}
}
