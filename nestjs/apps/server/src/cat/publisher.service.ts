import { connect, Channel } from "amqplib";
import { Injectable } from "@nestjs/common";
import { Cat } from "./cat.entity";

const AMQP_HOST = process.env.AMQP_HOST as string;
const AMQP_USER = process.env.AMQP_USER as string;
const AMQP_PWD = process.env.AMQP_PWD as string;
const rmqUrl = `amqp://${AMQP_USER}:${AMQP_PWD}@${AMQP_HOST}:5672`;
const EXCHANGE_NAME = process.env.RMQ_EXCHANGE as string;

@Injectable()
export class PublisherService {
  private channel: Channel;
  async init() {
    console.log(`⌛️ Connecting to Rabbit-MQ Server at ${rmqUrl}`);
    const connection = await connect(rmqUrl);
    console.log(`✅ Rabbit MQ Connection is ready`);
    this.channel = await connection.createChannel();
    console.log(`🛸 Created RabbitMQ Channel successfully`);
    await this.channel.assertExchange(EXCHANGE_NAME, "topic", {
      durable: true,
    });
  }

  publishCatCreated(cat: Cat) {
    this.channel.publish(
      EXCHANGE_NAME,
      'evt.cat.created',
      Buffer.from(JSON.stringify(cat)),
    );
    console.log('message published')
  }
}