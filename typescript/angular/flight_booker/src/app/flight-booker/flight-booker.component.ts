import {Component} from '@angular/core';
import {AbstractControl, FormControl, FormGroup, ValidationErrors, ValidatorFn, Validators} from "@angular/forms";
import {formatDate} from "@angular/common";


function returnNotBeforeDepartureValidator(): ValidatorFn {
  return (control: AbstractControl): ValidationErrors | null => {
    if (control.value.type === 'one-way flight') return null;

    const departure = Date.parse(control.value.departure);
    const return_ = Date.parse(control.value.return);
    if (return_ < departure) {
      return {return: "is before departure"}
    }
    return null;
  };
}

function parseableDateValidator(): ValidatorFn {
  return (ctrl: AbstractControl): ValidationErrors | null => {
    if (Date.parse(ctrl.value)) return null;

    return {invalid_date: true};
  }
}

@Component({
  selector: 'app-flight-booker',
  templateUrl: './flight-booker.component.html',
  styleUrls: ['./flight-booker.component.css']
})
export class FlightBookerComponent {
  types = ['one-way flight', 'return flight']
  form = new FormGroup({
    type: new FormControl<'one-way flight' | 'return flight'>('return flight', {nonNullable: true}),
    departure: new FormControl(this.fmtDate(new Date()), {
      nonNullable: true, validators: [Validators.required, parseableDateValidator()]
    }),
    return: new FormControl(this.fmtDate(new Date()), {validators: [parseableDateValidator()]}),
  }, {validators: [returnNotBeforeDepartureValidator()]});

  private fmtDate(date: Date): string {
    return formatDate(date, 'yyyy-MM-dd', 'en');
  }

}
