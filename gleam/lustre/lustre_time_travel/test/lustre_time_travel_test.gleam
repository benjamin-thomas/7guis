import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn package_compiles_test() {
  True
  |> should.be_true
}
