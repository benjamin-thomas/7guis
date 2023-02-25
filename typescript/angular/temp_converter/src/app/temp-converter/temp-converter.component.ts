import {Component} from '@angular/core';
import {FormControl, FormGroup} from "@angular/forms";

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
  celsiusError = '';
  fahrenheitError = '';

  /*
  NOTE:

  This is messy, I'll try better later. Maybe deriving values from rxjs constructs could help.
   */
  setFahrenheit() {
    const c_str = this.form.value.celsius;
    const c = Number(c_str!);
    if (c) {
      this.form.patchValue({
        fahrenheit: this.toFahrenheit(c).toString(),
      });
      this.celsiusError = '';
      this.fahrenheitError = ''; // clear possible previous error!
    } else {
      this.form.patchValue({fahrenheit: ''});
      if (!this.isEmpty(c_str)) {
        this.celsiusError = 'Invalid celsius value!';
      }
      if (this.isEmpty(this.form.value.fahrenheit)) {
        this.fahrenheitError = ''; // clear previous error, again!
      }
    }
  }

  setCelsius() {
    const f_str = this.form.value.fahrenheit;
    let f = Number(f_str!);
    if (f) {
      this.form.patchValue({
        celsius: this.toCelsius(f).toString(),
      });
      this.fahrenheitError = '';
      this.celsiusError = ''; // clear possible previous error!
    } else {
      this.form.patchValue({celsius: ''});
      if (!this.isEmpty(f_str)) {
        this.fahrenheitError = "Invalid fahrenheit value!";
      }
      if (this.isEmpty(this.form.value.celsius)) {
        this.celsiusError = ''; // clear previous error, again!
      }
    }
  }

  private toCelsius(fahrenheit: number) {
    // C = (F - 32) * (5/9)
    return (fahrenheit - 32) * 5 / 9;
  }

  private toFahrenheit(celsius: number) {
    // F = C * (9/5) + 32.
    return celsius * 9 / 5 + 32;
  }

  private isEmpty(val: string | null | undefined) {
    return val === null || val === undefined || val.trim() === '';
  }
}
