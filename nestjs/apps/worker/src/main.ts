import { NestFactory } from '@nestjs/core';
import { WorkerModule } from './worker.module';
import { RmqOptions, Transport } from '@nestjs/microservices';
import { RmqUrl } from '@nestjs/microservices/external/rmq-url.interface';

const AMQP_HOST = process.env.AMQP_HOST as string;
const AMQP_USER = process.env.AMQP_USER as string;
const AMQP_PWD = process.env.AMQP_PWD as string;
const rmqUrl = {
  protocol: 'amqp',
  hostname: AMQP_HOST,
  username: AMQP_USER,
  password: AMQP_PWD,
  heartbeat: 60,
} as RmqUrl
async function bootstrap() {
  const worker = await NestFactory.createMicroservice<RmqOptions>(WorkerModule, {
    transport: Transport.RMQ,
    options: {
      urls: [rmqUrl],
      queue: 'nestjs-evt.#',
      queueOptions: {
        durable: true,
      },
    },
  });
  await worker.listen();
}
bootstrap().catch(() => {});
