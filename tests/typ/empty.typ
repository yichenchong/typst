#table(
  columns: (1fr,) * 3,
  stroke: (x, y) => (
    outside: 3pt,
    inside: green,
    ..if y == 0 { (bottom: 2pt + red) },
    ..if y == 0 { (top: 1pt) },
    ..if x == 1 { (right: 3pt + blue) },
    ..if (x, y) == (2, 0) { (bottom: yellow) },
  ),
  ..([Hello],) * 10
)
