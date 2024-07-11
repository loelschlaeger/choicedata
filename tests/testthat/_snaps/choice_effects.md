# printing effects works

    Code
      choice_effects(choice_formula = choice_formula(formula = choice ~ price |
      income | comfort, error_term = "probit", random_effects = c("price+", "income")),
      choice_alternatives = choice_alternatives(J = 3))
    Message
      
      -- Choice effects 
    Output
        effect_name generic_name covariate alternative as_covariate as_effect
      1       ASC_B      alpha_1      <NA>           B        FALSE      TRUE
      2       ASC_C      alpha_2      <NA>           C        FALSE      TRUE
      3   comfort_A      alpha_3   comfort           A         TRUE      TRUE
      4   comfort_B      alpha_4   comfort           B         TRUE      TRUE
      5   comfort_C      alpha_5   comfort           C         TRUE      TRUE
      6    income_B          b_1    income           B        FALSE      TRUE
      7    income_C          b_2    income           C        FALSE      TRUE
      8       price          b_3     price        <NA>         TRUE     FALSE
        lc_effect     mixing
      1     FALSE       <NA>
      2     FALSE       <NA>
      3     FALSE       <NA>
      4     FALSE       <NA>
      5     FALSE       <NA>
      6     FALSE     normal
      7     FALSE     normal
      8     FALSE log-normal

