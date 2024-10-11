# printing effects works

    Code
      choice_effects(choice_formula = choice_formula(formula = choice ~ price |
      income | comfort, error_term = "probit", random_effects = c("price", "income")),
      choice_alternatives = choice_alternatives(J = 3))
    Message
      
      -- Choice effects 
    Output
        effect_name generic_name covariate alternative as_covariate as_effect mixing
      1       ASC_B      alpha_1      <NA>           B        FALSE      TRUE   <NA>
      2       ASC_C      alpha_2      <NA>           C        FALSE      TRUE   <NA>
      3   comfort_A      alpha_3   comfort           A         TRUE      TRUE   <NA>
      4   comfort_B      alpha_4   comfort           B         TRUE      TRUE   <NA>
      5   comfort_C      alpha_5   comfort           C         TRUE      TRUE   <NA>
      6       price          b_1     price        <NA>         TRUE     FALSE normal
      7    income_B          b_2    income           B        FALSE      TRUE normal
      8    income_C          b_3    income           C        FALSE      TRUE normal

