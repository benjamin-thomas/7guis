import {Component} from '@angular/core';
import {FormControl, FormGroup} from "@angular/forms";

type FormValue = string | null | undefined

@Component({
  selector: 'app-temp-converter',
  templateUrl: './temp-converter.component.html',
  styleUrls: ['./temp-converter.component.css']
})
export class TempConverterComponent {
  form = new FormGroup({
    celsius: new FormControl(''),
    fahrenheit: new FormControl(''),
  });
  errMsg = '';

  convert(f: (_: number) => number, formValue: FormValue): string {
    const c = Number(formValue!);
    if (c) {
      return f(c)?.toString();
    } else {
      return "";
    }
  }


  private setTemp(f: (_: number) => number, formValue: FormValue, errMsg: string, cb: (_: string) => void) {
    this.errMsg = ""; // Always reset previous errors first!
    const value = this.convert(f, formValue);
    if (!value && !this.isEmpty(formValue)) {
      this.errMsg = errMsg;
    }
    cb(value);
  }

  setFahrenheit() {
    this.setTemp(this.toFahrenheit, this.form.value.celsius, "Invalid celsius value!", (value: string) => {
      this.form.patchValue({fahrenheit: value});
    });
  }

  setCelsius() {
    this.setTemp(this.toCelsius, this.form.value.fahrenheit, "Invalid fahrenheit value!", (value: string) => {
      this.form.patchValue({celsius: value});
    });
  }


  toCelsius(fahrenheit: number) {
    return (fahrenheit - 32) * 5 / 9;
  }


  toFahrenheit(celsius: number) {
    return celsius * 9 / 5 + 32;
  }


  isEmpty(val: string | null | undefined) {
    return val === null || val === undefined || val.trim() === '';
  }
}
