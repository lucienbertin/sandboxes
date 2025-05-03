import { Injectable } from '@angular/core';
import { Apollo, gql } from 'apollo-angular';
import { map, Observable, tap } from 'rxjs';

export interface Cat {
  name: string;
  age: number;
  breed?: string;
}

const GET_CATS = gql`
  {
    cats {
      name
      age
      breed
    }
  }
`;
const CREATE_CAT = gql`
  mutation createCat($cat: CreateCatDto!) {
    createCat(cat: $cat) {
      name
      age
      breed
    }
  }
`;
@Injectable({
  providedIn: 'any',
})
export class CatsService {
  constructor(private readonly apollo: Apollo) {}

  public getCats(): Observable<Cat[]> {
    return this.apollo
      .query<{ cats: Cat[] }>({ query: GET_CATS })
      .pipe(map((r) => r.data.cats));
  }

  public createCat(cat: Cat) {
    return this.apollo
      .mutate({
        mutation: CREATE_CAT,
        variables: { cat },
      })
      .pipe(tap((r) => console.log(r)));
  }
}
