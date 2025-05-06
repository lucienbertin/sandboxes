import { Model } from 'mongoose';
import { Injectable } from '@nestjs/common';
import { InjectModel } from '@nestjs/mongoose';
import { Cat, CreateCatDto } from './cat.entity';

@Injectable()
export class DbService {
  constructor(@InjectModel(Cat.name) private catModel: Model<Cat>) {}

  async findAllCats(): Promise<Cat[]> {
    return this.catModel.find().exec();
  }

  async findCatByName(name: string): Promise<Cat | null> {
    return this.catModel
      .findOne({
        name: name,
      })
      .exec();
  }

  async createCat(createCatDto: CreateCatDto): Promise<Cat> {
    const createdCat = new this.catModel(createCatDto);
    return createdCat.save();
  }
}
