import { Test, TestingModule } from '@nestjs/testing';
import { PlaceService } from './place.service';
import { Repository } from 'typeorm';
import { Place } from './place.entity';
class MockPlaceRepo extends Repository<Place> {
  // eslint-disable-next-line @typescript-eslint/require-await
  public async find(): Promise<Place[]> {
    return [];
  }
}
describe('PlaceService', () => {
  let service: PlaceService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [        {
        provide: 'PlaceRepository',
        useClass: MockPlaceRepo,
      },
        PlaceService,
      ],
    }).compile();

    service = module.get<PlaceService>(PlaceService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
