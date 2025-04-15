import { MigrationInterface, QueryRunner } from "typeorm";

export class Initial1744707159613 implements MigrationInterface {
    name = 'Initial1744707159613'

    public async up(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`CREATE TABLE "posts" ("id" SERIAL NOT NULL, "title" text NOT NULL, "body" text NOT NULL, "published" boolean NOT NULL, CONSTRAINT "PK_2829ac61eff60fcec60d7274b9e" PRIMARY KEY ("id"))`);
    }

    public async down(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`DROP TABLE "posts"`);
    }

}
