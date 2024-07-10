# choice_formula can be printed

    Code
      choice_formula(formula = choice ~ A | B, re = NULL)
    Message
      
      -- Choice formula 
      * choice ~ A | B
      * error term: probit

---

    Code
      choice_formula(formula = choice ~ A + B, re = c("A+", "B"))
    Message
      
      -- Choice formula 
      * choice ~ A + B
      * error term: probit
      * random effects:
        1. A: log-normal
        2. B: normal

