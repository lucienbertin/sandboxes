import client, { Channel } from "amqplib";
const AMQP_URL = process.env.AMQP_URL as string;
const EXCHANGE_NAME = process.env.RMQ_EXCHANGE as string;
class RabbitMQPubConnection {
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

      this.channel.assertExchange(EXCHANGE_NAME, "topic");

      this.connected = true;
    } catch (error) {
      console.error(error);
      console.error(`Not connected to MQ Server`);
    } finally {
      return this;
    }
  }
}

const mqPubConnection = new RabbitMQPubConnection();
const isPubInitialized = mqPubConnection
  .initialize()
  .then((conn) => conn.connected);

export async function publish(
  routingKey: string,
  message: unknown,
): Promise<void> {
  await isPubInitialized;

  await mqPubConnection.channel.publish(
    EXCHANGE_NAME,
    routingKey,
    Buffer.from(JSON.stringify(message)),
  );
}
