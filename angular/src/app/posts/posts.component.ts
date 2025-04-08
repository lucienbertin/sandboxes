import { Component, inject } from '@angular/core';
import { Post, PostsService } from './posts.service';
import { CommonModule } from '@angular/common';

@Component({
  standalone: true,
  selector: 'app-posts',
  imports: [
    CommonModule,
  ],
  templateUrl: './posts.component.html',
  styleUrl: './posts.component.scss'
})
export class PostsComponent {
  private service = inject(PostsService);

  posts$ = this.service.getPosts();
}
