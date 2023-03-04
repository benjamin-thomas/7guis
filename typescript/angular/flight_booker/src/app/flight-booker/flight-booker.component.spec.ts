import { ComponentFixture, TestBed } from '@angular/core/testing';

import { FlightBookerComponent } from './flight-booker.component';

describe('FlightBookerComponent', () => {
  let component: FlightBookerComponent;
  let fixture: ComponentFixture<FlightBookerComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ FlightBookerComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(FlightBookerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
