import { registerConsumer, rustPostConsumer } from "@/consumers";

setTimeout(() => registerConsumer(rustPostConsumer), 0);