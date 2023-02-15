#let gray(v) = rgb(0, 0, 0, v)
#set page(height: 110pt)
#table(
  columns: (1fr,) * 3,
  align: center + horizon,
  stroke: (x, y) => (
    rest: 4pt + gray(40%),
    ..if (x, y) == (1, 1) { (right: yellow) },
    ..if (x, y) == (1, 4) { (left: red) },
    ..if y in (2, 3) { (y: 2pt + blue) },
  ),
  ..([Hello #v(12pt, weak: true) World],) * 15
)
