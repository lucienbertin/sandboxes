import 'dotenv/config';
import { NestFactory } from '@nestjs/core';
import {
  FastifyAdapter,
  NestFastifyApplication,
} from '@nestjs/platform-fastify';
import { ServerModule } from './server.module';

async function bootstrap() {
  const server = await NestFactory.create<NestFastifyApplication>(
    ServerModule,
    new FastifyAdapter(),
  );
  server.enableCors({
    allowedHeaders: '*',
    origin: '*',
    methods: 'GET,HEAD,PUT,PATCH,POST,DELETE',
  });
  await server.listen(process.env.PORT as string, process.env.HOST as string);
}
bootstrap().catch(() => {});
