# choice_formula can be printed

    Code
      choice_formula(formula = choice ~ A | B, re = NULL, ordered = FALSE)
    Message
      
      -- Choice model formula 
    Output
      choice ~ A | B

---

    Code
      choice_formula(formula = choice ~ A + B, re = c("A+", "B"), ordered = TRUE)
    Message
      
      -- Choice model formula 
    Output
      choice ~ A + B
      with random effects
      * A+
      * B

