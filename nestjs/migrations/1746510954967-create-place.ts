import { MigrationInterface, QueryRunner } from "typeorm";

export class CreatePlace1746510954967 implements MigrationInterface {
    name = 'CreatePlace1746510954967'

    public async up(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`CREATE TABLE "place" ("id" integer NOT NULL, "name" text NOT NULL, "point" geometry(Point,4326) NOT NULL, CONSTRAINT "PK_96ab91d43aa89c5de1b59ee7cca" PRIMARY KEY ("id"))`);
    }

    public async down(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`DROP TABLE "place"`);
    }

}
