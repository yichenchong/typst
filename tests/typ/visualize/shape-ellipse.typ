// Test the `ellipse` function.

---
// Default ellipse.
#ellipse()

---
#set rect(inset: 0pt)
#set ellipse(inset: 0pt)

Rect in ellipse in fixed rect.
#rect(fill: rgb("2a631a"),
  ellipse(fill: forest, stroke: 4pt + teal,
    rect(fill: conifer, inset: 4pt,
      align(center + horizon)[
        Stuff in \
        the ellipse!
      ]
    )
  )
)

Auto-sized ellipse.
#ellipse(fill: conifer, stroke: 3pt + forest, inset: 3pt)[
  #set text(8pt)
  But, soft! what light through yonder window breaks?
]


An inline
#box(ellipse(width: 8pt, height: 6pt, outset: (top: 3pt, rest: 5.5pt)))
ellipse.
