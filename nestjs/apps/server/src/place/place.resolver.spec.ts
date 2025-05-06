import { Test, TestingModule } from '@nestjs/testing';
import { PlaceResolver } from './place.resolver';
import { PlaceService } from './place.service';

class MockService {}
describe('PlaceResolver', () => {
  let resolver: PlaceResolver;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        PlaceResolver,
        {
          provide: PlaceService,
          useClass: MockService,
        },
      ],
    }).compile();

    resolver = module.get<PlaceResolver>(PlaceResolver);
  });

  it('should be defined', () => {
    expect(resolver).toBeDefined();
  });
});
