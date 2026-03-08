let useReducer: (
  'model,
  ('model, 'msg) => ('model, array<'effect>),
  ('msg => unit, 'effect) => unit,
) => ('model, 'msg => unit)
