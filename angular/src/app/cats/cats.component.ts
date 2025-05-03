import { Component, inject } from '@angular/core';
import { Cat, CatsService } from './cats.service';
import { Observable } from 'rxjs';
import { CommonModule } from '@angular/common';

@Component({
  standalone: true,
  selector: 'app-cats',
  imports: [CommonModule],
  templateUrl: './cats.component.html',
  styleUrl: './cats.component.scss',
})
export class CatsComponent {
  private service = inject(CatsService);

  cats$: Observable<Cat[]> = this.service.getCats();

  createZebro() {
    const zebro = {
      name: 'zebro',
      age: 6,
      breed: 'tigré',
    };
    this.service.createCat(zebro).subscribe(() => console.log('zebro created'));
  }
}
