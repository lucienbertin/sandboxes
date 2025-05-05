import { Controller } from '@nestjs/common';
import { Ctx, MessagePattern, Payload, RmqContext } from '@nestjs/microservices';

@Controller()
export class WorkerController {
    constructor() {
        console.log("constructing worker controller")
    }

    @MessagePattern()
    async recieve(@Payload() data: any, @Ctx() context: RmqContext) {
        console.log('msg routing key:', context.getPattern());
      console.log('msg data:', data);
    }
}
