#let gray(v) = rgb(0, 0, 0, v)
#set page(height: 110pt)
#table(
  columns: (1fr,) * 3,
  align: center + horizon,
  stroke: (x, y) => (
    rest: 4pt + gray(40%),
    ..if (x, y) == (1, 1) { (right: yellow) },
    ..if (x, y) == (1, 3) { (left: red) },
    ..if y == 2 { (y: 2pt + blue) },
  ),
  ..([Hello #v(12pt, weak: true) World],) * 15
)
