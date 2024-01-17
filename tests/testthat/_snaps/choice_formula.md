# choice_formula can be printed

    Code
      choice_formula(formula = choice ~ A | B, re = NULL, ordered = FALSE)
    Output
      Model formula: choice ~ A | B

---

    Code
      choice_formula(formula = choice ~ A + B, re = c("A+", "B"), ordered = TRUE)
    Output
      Model formula: choice ~ A + B
      Random effects: A+ B

