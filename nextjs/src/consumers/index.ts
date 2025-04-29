import client, { Channel, ConsumeMessage } from "amqplib";

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
      console.log(msg.fields.routingKey);
      console.log(msg.content.toString());

      chann.ack(msg);
    }
  };

  return partial;
}

export const rustPostConsumer = {
  exchange: "rust",
  routingKey: "evt.post.#",
  handler: logger,
} as RmqConsumer;
