import { MigrationInterface, QueryRunner } from "typeorm";

export class RemPostPublished1746433942719 implements MigrationInterface {
    name = 'RemPostPublished1746433942719'

    public async up(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`ALTER TABLE "posts" DROP COLUMN "published"`);
    }

    public async down(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`ALTER TABLE "posts" ADD "published" boolean NOT NULL`);
    }

}
