#table(
  columns: (1fr,) * 3,
  // fill: (x, y) => if y == 0 { teal.lighten(50%) },
  // stroke: 3pt,
  // stroke: (inside: 1pt),
  // stroke: (outside: 1pt),
  // stroke: (left: 2pt, right: red),
  // stroke: (outside: 2pt, inside: 1pt),
  // stroke: (outside: 2pt, inside: 1pt),
  // stroke: (x, y) => (
  //   outside: 2pt,
  //   inside: 1pt,
  //   ..if y == 0 { (bottom: 2pt) },
  // ),
  // stroke: (left: red, right: 3pt, outside: 2pt, top: none),
  // stroke: (x, y) => (
  //   outside: 3pt,
  //   inside: green,
  //   ..if y == 0 { (bottom: 2pt + red) },
  //   ..if y == 0 { (top: 1pt) },
  //   ..if x == 1 { (right: 3pt + blue) },
  //   ..if (x, y) == (2, 0) { (bottom: yellow) },
  // ),
  // stroke: none,
  ..([Hello],) * 10
)
