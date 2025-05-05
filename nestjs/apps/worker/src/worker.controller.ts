import { Controller } from '@nestjs/common';
import { Ctx, MessagePattern, Payload, RmqContext } from '@nestjs/microservices';
import { WorkerService } from './worker.service';
import { Post } from './models';

@Controller()
export class WorkerController {
    constructor(private service: WorkerService) {}
    // for some reason i cant make the nestjs mrq microservice work for me
    // the context.pattern is always undefined
    // so for now i get this controller to catch all and i'll redispatch in a switch
    // thats not what i want to do but its what im gonna do
    @MessagePattern()
    async handler(@Payload() payload: any, @Ctx() context: RmqContext) {
        const routingKey = context.getArgByIndex(0).fields.routingKey; // i got to go fetch it
        switch (routingKey) {
            case "evt.post.published":
            case "evt.post.updated":
                return await this.handlePostModified(payload);
            case "evt.post.deleted":
                return await this.handlePostDeleted(payload);
            default:
                console.log(`no handler for routingKey '${routingKey}' | payload: `, payload)
        }
    }

    async handlePostModified(post: Post) {
        console.log(`upserting post | post: `, post)
        this.service.upsertPost(post);
    }

    async handlePostDeleted(post: Post) {

    }
}
