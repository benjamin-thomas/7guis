import { Component } from '@angular/core';

@Component({
  selector: 'app-counter',
  templateUrl: './counter.component.html',
  styleUrls: ['./counter.component.css']
})
export class CounterComponent {
  get counter(): number {
    return this._counter;
  }
  private _counter = 0;

  inc() {
    this._counter++
  }
}
