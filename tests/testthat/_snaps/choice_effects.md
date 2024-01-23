# effect overview can be created

    Code
      choice_effects(choice_formula = choice_formula(formula = choice ~ price |
      income | comfort, re = c("price+", "income")), choice_alternatives = choice_alternatives(
        J = 3))
    Message
      
      -- Choice effects 
    Output
             name covariate alternative as_covariate as_effect     mixing
      1     ASC_B      <NA>           B        FALSE      TRUE       none
      2     ASC_C      <NA>           C        FALSE      TRUE       none
      3 comfort_A   comfort           A         TRUE      TRUE       none
      4 comfort_B   comfort           B         TRUE      TRUE       none
      5 comfort_C   comfort           C         TRUE      TRUE       none
      6  income_B    income           B        FALSE      TRUE     normal
      7  income_C    income           C        FALSE      TRUE     normal
      8     price     price        <NA>         TRUE     FALSE log-normal

