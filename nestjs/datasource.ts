import { typeOrmConfig } from "./src/typeorm.options";
import { DataSource, DataSourceOptions } from "typeorm";

// used by typeorm cli
export const connectionSource = new DataSource({
  ...typeOrmConfig,
  entities: ['src/**/*.entity.ts'],
  migrations: ['migrations/*{.ts,.js}'],
  autoLoadEntities: false,
  readonly: false,
} as DataSourceOptions);
  