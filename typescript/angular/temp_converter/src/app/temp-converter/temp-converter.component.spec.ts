import {ComponentFixture, TestBed} from '@angular/core/testing';

import {TempConverterComponent} from './temp-converter.component';
import {ReactiveFormsModule} from "@angular/forms";

describe('TempConverterComponent', () => {
  let component: TempConverterComponent;
  let fixture: ComponentFixture<TempConverterComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [TempConverterComponent],
      imports: [ReactiveFormsModule],
    })
      .compileComponents();

    fixture = TestBed.createComponent(TempConverterComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
