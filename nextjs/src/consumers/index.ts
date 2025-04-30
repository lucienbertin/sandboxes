import client, { Channel, ConsumeMessage } from "amqplib";
import { AgentDelegate, Post, Worker, AgentType } from "@/domain";
import * as domain from "@/domain";
import * as db from "@/infrastructure/db";

const AMQP_URL = process.env.AMQP_URL as string;

class RabbitMQSubConnection {
  channel!: Channel;
  connected: boolean = false;

  async initialize(): Promise<this> {
    if (this.connected && this.channel) return this;

    try {
      console.log(`⌛️ Connecting to Rabbit-MQ Server`);

      const connection = await client.connect(AMQP_URL);
      console.log(`✅ Rabbit MQ Connection is ready`);

      this.channel = await connection.createChannel();
      console.log(`🛸 Created RabbitMQ Channel successfully`);

      this.connected = true;
    } catch (error) {
      console.error(error);
      console.error(`Not connected to MQ Server`);
    } finally {
      return this;
    }
  }
}

const mqSubConnection = new RabbitMQSubConnection();
const isSubInitialized = mqSubConnection
  .initialize()
  .then((conn) => conn.connected);

export type RmqConsumer = {
  exchange: string;
  routingKey: string;
  handler(chann: Channel): (msg: ConsumeMessage | null) => void;
};

export async function registerConsumer(consumer: RmqConsumer) {
  await isSubInitialized;

  console.log(`i want to register consumer`);
  console.log(consumer);

  const queue = `nextjs-${consumer.routingKey}`;
  console.log(`asserting queue ${queue} `);
  await mqSubConnection.channel.assertQueue(queue, { durable: true });
  console.log(`queue ${queue} is asserted`);

  await mqSubConnection.channel.bindQueue(
    queue,
    consumer.exchange,
    consumer.routingKey,
  );
  console.log(`queue is bound`);

  await mqSubConnection.channel.consume(
    queue,
    consumer.handler(mqSubConnection.channel),
  );
  console.log(`consumer is registered`);
}

function logger(chann: Channel): (msg: ConsumeMessage | null) => void {
  const partial = (msg: ConsumeMessage | null) => {
    if (msg !== null) {
      const payload = JSON.parse(msg.content.toString());
      const post = {
        id: payload.id,
        title: payload.title,
        body: payload.body,
        author: `${payload.author.first_name} ${payload.author.last_name}`,
      } as Post;
      
      console.log(msg.fields.routingKey);
      console.log(payload);
      console.log(post);

      chann.ack(msg);
    }
  };

  return partial;
}

function updater(chann: Channel): (msg: ConsumeMessage | null) => void {
  const partial = async (msg: ConsumeMessage | null) => {
    if (msg !== null) {
      const payload = JSON.parse(msg.content.toString());
      const post = {
        id: payload.id,
        title: payload.title,
        body: payload.body,
        author: `${payload.author.first_name} ${payload.author.last_name}`,
      } as Post;
      console.log(`recieved ${msg.fields.routingKey} with post id ${post.id}`);

      switch (msg.fields.routingKey) {
        case 'evt.post.deleted':
          await domain.deletePost(iAmWorker, db.deletePost)(post);
          break;
        case 'evt.post.updated':
        case 'evt.post.published':
        default:
          await domain.upsertPost(iAmWorker, db.upsertPost)(post);
          break;
      }
      console.log(`✅ ack`);
      chann.ack(msg);
    }
  };

  return partial;
}

export const rustPostConsumer = {
  exchange: "rust",
  routingKey: "evt.post.#",
  handler: updater,
} as RmqConsumer;

const iAmWorker: AgentDelegate = async () => ({ _type: AgentType.Worker } as Worker);
