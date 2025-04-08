import { HttpClient, HttpHeaders } from '@angular/common/http';
import { inject, Injectable } from '@angular/core';

export interface Post {
  id: number;
  title: string;
  body: string;
}

// john@d.oe -> eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJqb2huQGQub2UifQ.g9sqFgZmzwj2wcD_IJ7_A8ULSqeOblsTbKq2V1j5uVw
// lucien@bert.in -> eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJsdWNpZW5AYmVydC5pbiJ9.MQ2AtPRuMhZuu84jFpjbnZF3tMREpSi51YEU6yq8KBI
const bearer = `Bearer eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJsdWNpZW5AYmVydC5pbiJ9.MQ2AtPRuMhZuu84jFpjbnZF3tMREpSi51YEU6yq8KBI`;
const headers = new HttpHeaders({ Authorization: bearer });

@Injectable({
  providedIn: 'any',
})
export class PostsService {
  private http = inject(HttpClient);

  public getPosts() {
    return this.http.get<Post[]>('http://127.0.0.1:8000/api/posts', {
      headers,
    });
  }
}
