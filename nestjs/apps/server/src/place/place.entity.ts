import { Column, Entity, Point, PrimaryColumn } from 'typeorm';
import { Field, Int, ObjectType } from '@nestjs/graphql';

@ObjectType()
@Entity()
export class Place {
  @Field(() => Int)
  @PrimaryColumn()
  id: number;

  @Field()
  @Column({ type: 'text' })
  name: string;

  @Column({
    type: 'geometry',
    srid: 4326,
    spatialFeatureType: 'Point',
  })
  point: Point;
}
