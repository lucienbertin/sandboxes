import { Routes } from '@angular/router';
import { PostsComponent } from './posts';

export const routes: Routes = [
  { path: 'posts', component: PostsComponent },
  {
    path: 'posts-ll',
    loadComponent: () => import('./posts').then((m) => m.PostsComponent),
  },
  {
    path: 'cats',
    loadComponent: () => import('./cats').then((m) => m.CatsComponent),
  },
  { path: '**', redirectTo: '/cats' },
];
