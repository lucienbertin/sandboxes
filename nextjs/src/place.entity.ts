import { Column, Entity, PrimaryGeneratedColumn } from 'typeorm';

export interface IPlace {
  id: number;
  name: string;
}

@Entity({ name: 'places' })
export class Place implements IPlace {
  @PrimaryGeneratedColumn()
  id!: number;

  @Column({ type: 'text' })
  name!: string;

  asStruct(): IPlace {
    return { ...this } as IPlace;
  }
}
