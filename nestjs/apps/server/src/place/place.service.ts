import { Injectable } from '@nestjs/common';
import { Place } from './place.entity';
import { InjectRepository } from '@nestjs/typeorm';
import { Repository } from 'typeorm';

@Injectable()
export class PlaceService {
  constructor(
    @InjectRepository(Place) private readonly repo: Repository<Place>,
  ) {}

  findAll(): Promise<Place[]> {
    return this.repo.find();
  }

  findOneById(id: number): Promise<Place | null> {
    return this.repo.findOneBy({ id: id });
  }
}
