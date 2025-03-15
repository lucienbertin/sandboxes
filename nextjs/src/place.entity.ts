import * as typeorm from 'typeorm';
import { Feature, GeoJSON, Point } from 'geojson';

export interface IPlace {
  id: number;
  name: string;
  asGeoJSON(): GeoJSON<Point, IPlace>;
}

@typeorm.Entity({ name: 'places' })
export class Place implements IPlace {
  @typeorm.PrimaryGeneratedColumn()
  id!: number;

  @typeorm.Column({ type: 'text' })
  name!: string;

  @typeorm.Column({
    type: 'geography',
    srid: 4326,
    spatialFeatureType: 'Point',
  })
  geometry!: typeorm.Point;

  asStruct(): IPlace {
    return { ...this } as IPlace;
  }
  asGeoJSON(): Feature<Point, IPlace> {
    return {
      id: this.id,
      type: 'Feature',
      properties: this.asStruct(),
      geometry: this.geometry,
    }
  }
}
