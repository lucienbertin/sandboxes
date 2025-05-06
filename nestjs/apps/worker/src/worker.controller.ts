import { Controller } from '@nestjs/common';
import {
  Ctx,
  MessagePattern,
  Payload,
  RmqContext,
} from '@nestjs/microservices';
import { WorkerService } from './worker.service';
import { Place, Post } from './models';
import { Feature, Point } from 'geojson';

@Controller()
export class WorkerController {
  constructor(private service: WorkerService) {}
  // for some reason i cant make the nestjs mrq microservice work for me
  // the context.pattern is always undefined
  // so for now i get this controller to catch all and i'll redispatch in a switch
  // thats not what i want to do but its what im gonna do
  @MessagePattern()
  async handler(@Payload() payload: unknown, @Ctx() context: RmqContext) {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
    const routingKey = context.getArgByIndex(0).fields.routingKey as string; // i got to go fetch it
    switch (routingKey) {
      case 'evt.post.published':
      case 'evt.post.updated':
        return await this.handlePostModified(payload as Post);
      case 'evt.post.deleted':
        return await this.handlePostDeleted(payload as Post);
      case 'evt.place.created':
        return await this.handlePlaceModified(payload as Feature<Point, Place>);
      default:
        console.log(
          `no handler for routingKey '${routingKey}' | payload: `,
          payload,
        );
    }
  }

  // POSTS
  async handlePostModified(post: Post) {
    await this.service.upsertPost(post);
  }

  async handlePostDeleted(post: Post) {
    await this.service.deletePost(post);
  }

  // PLACES
  async handlePlaceModified(placeFeature: Feature<Point, Place>) {
    await this.service.upsertPlace(placeFeature);
  }
}
