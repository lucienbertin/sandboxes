import { Model } from 'mongoose';
import { Injectable } from '@nestjs/common';
import { InjectModel } from '@nestjs/mongoose';
import { Cat, CreateCatDto } from './cat.entity';

@Injectable()
export class CatService {
  constructor(@InjectModel(Cat.name) private catModel: Model<Cat>) {}

  async findAll(): Promise<Cat[]> {
    return this.catModel.find().exec();
  }

  async findByName(name: string): Promise<Cat | null> {
    return this.catModel.findOne({
      name: name
    }).exec();
  }

  async create(createCatDto: CreateCatDto): Promise<Cat> {
    const createdCat = new this.catModel(createCatDto);
    return createdCat.save();
  }
}
