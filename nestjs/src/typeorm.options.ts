export const typeOrmConfig = {
  type: 'postgres',
  host: process.env.DB_HOST,
  port: 5432,
  password: 'nestjs',
  username: 'nestjs',
  database: 'nestjs-db',
  autoLoadEntities: true,
  synchronize: false,
};
