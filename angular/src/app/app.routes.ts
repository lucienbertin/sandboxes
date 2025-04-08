import { Routes } from '@angular/router';
import { PostsComponent } from './posts';

export const routes: Routes = [
    { path: 'posts', component: PostsComponent },
    { path: 'posts-ll', loadComponent: () => import('./posts').then(child => child.PostsComponent) },
    { path: '**', redirectTo: '/posts' },
];
