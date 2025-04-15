import { DataSource, DataSourceOptions } from 'typeorm';

export const typeOrmConfig = {
  type: 'postgres',
  host: 'localhost',
  port: 5432,
  password: 'nestjs',
  username: 'nestjs',
  database: 'nestjs',
  autoLoadEntities: true,
  synchronize: false,
};
