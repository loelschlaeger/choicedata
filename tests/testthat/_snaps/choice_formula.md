# choice_formula can be printed

    Code
      choice_formula(formula = choice ~ A | B, error_term = "probit", random_effects = character())
    Message
      
      -- Choice formula 
      * choice ~ A | B
      * error term: probit

---

    Code
      choice_formula(formula = choice ~ A + B, error_term = "probit", random_effects = c(
        "A", "B"))
    Message
      
      -- Choice formula 
      * choice ~ A + B
      * error term: probit
      * random effects:
        * A: normal
        * B: normal

