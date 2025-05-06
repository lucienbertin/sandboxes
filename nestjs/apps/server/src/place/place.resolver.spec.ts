import { Test, TestingModule } from '@nestjs/testing';
import { PlaceResolver } from './place.resolver';
import { PlaceService } from './place.service';

describe('PlaceResolver', () => {
  let resolver: PlaceResolver;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [PlaceResolver, PlaceService],
    }).compile();

    resolver = module.get<PlaceResolver>(PlaceResolver);
  });

  it('should be defined', () => {
    expect(resolver).toBeDefined();
  });
});
