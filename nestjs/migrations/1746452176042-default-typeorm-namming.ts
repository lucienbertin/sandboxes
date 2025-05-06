import { MigrationInterface, QueryRunner } from "typeorm";

export class DefaultTypeormNamming1746452176042 implements MigrationInterface {
    name = 'DefaultTypeormNamming1746452176042'

    public async up(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`CREATE TABLE "post" ("id" integer NOT NULL, "title" text NOT NULL, "body" text NOT NULL, "authorEmail" character varying, CONSTRAINT "PK_be5fda3aac270b134ff9c21cdee" PRIMARY KEY ("id"))`);
        await queryRunner.query(`ALTER TABLE "post" ADD CONSTRAINT "FK_7950a9445698c03ddfa1c01c2a3" FOREIGN KEY ("authorEmail") REFERENCES "author"("email") ON DELETE NO ACTION ON UPDATE NO ACTION`);
    }

    public async down(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`ALTER TABLE "post" DROP CONSTRAINT "FK_7950a9445698c03ddfa1c01c2a3"`);
        await queryRunner.query(`DROP TABLE "post"`);
    }

}
