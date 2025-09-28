# choice_formula can be printed

    Code
      choice_formula(formula = choice ~ A | B)
    Message
      
      -- Choice formula 
      * choice ~ A | B | 0
      * error term: probit

---

    Code
      choice_formula(formula = choice ~ A + B, random_effects = c(A = "cn", B = "cn"))
    Message
      
      -- Choice formula 
      * choice ~ A + B | 1 | 0
      * error term: probit
      * random effects:
        * A: cn
        * B: cn

