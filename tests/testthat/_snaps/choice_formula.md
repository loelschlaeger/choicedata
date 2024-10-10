# choice_formula can be printed

    Code
      choice_formula(formula = choice ~ A | B, error_term = "logit", random_effects = character())
    Message
      
      -- Choice formula 
      * choice ~ A | B
      * error term: logit

---

    Code
      choice_formula(formula = choice ~ A + B, error_term = "probit", random_effects = c(
        "A+", "B"))
    Message
      
      -- Choice formula 
      * choice ~ A + B
      * error term: probit
      * random effects:
        * A: log-normal
        * B: normal

---

    Code
      choice_formula(formula = choice ~ A + B, error_term = "logit", random_effects = c(
        "A+", "B"), latent_class = c("A", "B"))
    Message
      
      -- Choice formula 
      * choice ~ A + B
      * error term: logit
      * random effects:
        * A: log-normal
        * B: normal
      * latent classes:
        * A
        * B

