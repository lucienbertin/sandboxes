import { MigrationInterface, QueryRunner } from "typeorm";

export class AddPostAuthor1746449430256 implements MigrationInterface {
    name = 'AddPostAuthor1746449430256'

    public async up(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`CREATE TABLE "author" ("firstName" character varying NOT NULL, "lastName" character varying NOT NULL, "email" character varying NOT NULL, CONSTRAINT "PK_384deada87eb62ab31c5d5afae5" PRIMARY KEY ("email"))`);
        await queryRunner.query(`ALTER TABLE "posts" ADD "authorEmail" character varying`);
        await queryRunner.query(`ALTER TABLE "posts" ALTER COLUMN "id" DROP DEFAULT`);
        await queryRunner.query(`DROP SEQUENCE "posts_id_seq"`);
        await queryRunner.query(`ALTER TABLE "posts" ADD CONSTRAINT "FK_35349cd5348979e75ce30bb2dd8" FOREIGN KEY ("authorEmail") REFERENCES "author"("email") ON DELETE NO ACTION ON UPDATE NO ACTION`);
    }

    public async down(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`ALTER TABLE "posts" DROP CONSTRAINT "FK_35349cd5348979e75ce30bb2dd8"`);
        await queryRunner.query(`CREATE SEQUENCE IF NOT EXISTS "posts_id_seq" OWNED BY "posts"."id"`);
        await queryRunner.query(`ALTER TABLE "posts" ALTER COLUMN "id" SET DEFAULT nextval('"posts_id_seq"')`);
        await queryRunner.query(`ALTER TABLE "posts" DROP COLUMN "authorEmail"`);
        await queryRunner.query(`DROP TABLE "author"`);
    }

}
