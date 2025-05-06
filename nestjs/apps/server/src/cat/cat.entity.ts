import { Prop, Schema, SchemaFactory } from '@nestjs/mongoose';
import { HydratedDocument } from 'mongoose';
import { Field, InputType, Int, ObjectType } from '@nestjs/graphql';

export type CatDocument = HydratedDocument<Cat>;

@ObjectType()
@Schema()
export class Cat {
  @Field()
  @Prop()
  name: string;

  @Field(() => Int)
  @Prop()
  age: number;

  @Field({ nullable: true })
  @Prop()
  breed: string;
}

export const CatSchema = SchemaFactory.createForClass(Cat);

@InputType()
export class CreateCatDto {
  @Field()
  name: string;
  @Field()
  age: number;
  @Field({ nullable: true })
  breed?: string;
}
