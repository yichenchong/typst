#let gray(v) = rgb(0, 0, 0, v)
#set page(height: auto /* 110pt */)
#table(
  columns: (1fr,) * 3,
  rows: 1cm,
  align: center + horizon,
  inset: 2pt,
  stroke: (x, y) => (
    rest: 8pt + gray(40%),
    ..if (x, y) == (1, 1) { (right: yellow) },
    ..if (x, y) == (1, 4) { (left: red) },
    ..if y in (2, 3) { (y: none) },
  ),
  ..(rect(width: 100%, height: 100%, fill: rgb("#7fdbff55"))/*[Hello #v(12pt, weak: true) World]*/,) * 15
)
