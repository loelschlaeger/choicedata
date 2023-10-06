# probit_formula can be printed

    Code
      probit_formula(formula = choice ~ A | B, re = NULL, ordered = FALSE)
    Output
      Model formula: choice ~ A | B

---

    Code
      probit_formula(formula = choice ~ A + B, re = c("A+", "B"), ordered = TRUE)
    Output
      Model formula: choice ~ A + B
      Random effects: A+ B

