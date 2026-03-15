export const assertNever = (x: never): never => {
  throw new Error(`Unexpected value: ${JSON.stringify(x)}`)
}
