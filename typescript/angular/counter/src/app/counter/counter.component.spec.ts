import {ComponentFixture, TestBed} from '@angular/core/testing';

import {CounterComponent} from './counter.component';

describe('CounterComponent', () => {
  let component: CounterComponent;
  let fixture: ComponentFixture<CounterComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [CounterComponent]
    })
      .compileComponents();

    fixture = TestBed.createComponent(CounterComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should increase its counter on button click', function () {
    const button = fixture.debugElement.nativeElement.querySelector('button');
    expect(component.counter).toEqual(0);
    button.click();
    expect(component.counter).toEqual(1);
  });
});
