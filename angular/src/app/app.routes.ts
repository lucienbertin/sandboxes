import { Routes } from '@angular/router';
import { PostsComponent } from './posts';

export const routes: Routes = [
    { path: 'posts', component: PostsComponent },
    { path: '**', redirectTo: '/posts' },
];
