# printing effects works

    Code
      choice_effects(choice_formula = choice_formula(formula = choice ~ price |
      income | comfort, error_term = "probit", random_effects = c("price+", "income")),
      choice_alternatives = choice_alternatives(J = 3))
    Message
      
      -- Choice effects 
    Output
             name covariate alternative as_covariate as_effect     mixing lc_effect
      1     ASC_B      <NA>           B        FALSE      TRUE       <NA>     FALSE
      2     ASC_C      <NA>           C        FALSE      TRUE       <NA>     FALSE
      3 comfort_A   comfort           A         TRUE      TRUE       <NA>     FALSE
      4 comfort_B   comfort           B         TRUE      TRUE       <NA>     FALSE
      5 comfort_C   comfort           C         TRUE      TRUE       <NA>     FALSE
      6  income_B    income           B        FALSE      TRUE     normal     FALSE
      7  income_C    income           C        FALSE      TRUE     normal     FALSE
      8     price     price        <NA>         TRUE     FALSE log-normal     FALSE

