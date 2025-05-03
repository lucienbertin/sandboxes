import { Injectable } from '@angular/core';
import { Apollo, gql } from 'apollo-angular';
import { map, Observable, of } from 'rxjs';

export interface Cat {
  name: string;
  age: number;
}

const GET_CATS = gql`
  {
    cats {
      name
      age
    }
  }
`;
@Injectable({
  providedIn: 'any'
})
export class CatsService {

  constructor(private readonly apollo: Apollo) { }

  public getCats(): Observable<Cat[]> {
    // return of([{ name: "grisou", age: 9 }, { name: "zebro", age: 6}]);
    return this.apollo.query<Cat[]>({ query: GET_CATS }).pipe(
      map(r => r.data)
    );
  }
}
