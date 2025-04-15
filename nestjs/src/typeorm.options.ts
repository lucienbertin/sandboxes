import { registerAs } from "@nestjs/config";
import { DataSource, DataSourceOptions } from "typeorm";

export const typeOrmConfig = {
    type: 'postgres',
    host: 'localhost',
    port: 5432,
    password: 'nestjs',
    username: 'nestjs',
    database: 'nestjs',
    autoLoadEntities: true,
    synchronize: false,
}

// used by nestjs so no 
// export default registerAs('typeorm', () => typeOrmConfig);

// used by typeorm cli
export const connectionSource = new DataSource({
    ...typeOrmConfig,
    entities: ["src/**/*.entity.ts"],
    migrations: ["src/migrations/*{.ts,.js}"],
    autoLoadEntities: false,
} as DataSourceOptions);
