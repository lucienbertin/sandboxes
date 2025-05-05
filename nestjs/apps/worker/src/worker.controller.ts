import { Controller } from '@nestjs/common';
import { Ctx, MessagePattern, Payload, RmqContext } from '@nestjs/microservices';

@Controller()
export class WorkerController {
    @MessagePattern()
    async recieve(@Payload() data: any, @Ctx() context: RmqContext) {
        console.log('msg routing key:', context.getArgByIndex(0).fields.routingKey);
        console.log('msg data:', data);
    }
}
