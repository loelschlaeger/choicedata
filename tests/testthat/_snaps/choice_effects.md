# printing effects works

    Code
      choice_effects(choice_formula = choice_formula(formula = choice ~ price |
      income | comfort, random_effects = c(price = "cn", income = "cn")),
      choice_alternatives = choice_alternatives(J = 3))
    Message
      
      -- Choice effects 
    Output
        effect_name generic_name covariate alternative as_covariate as_effect mixing
      1       ASC_B       beta_1      <NA>           B        FALSE      TRUE   <NA>
      2       ASC_C       beta_2      <NA>           C        FALSE      TRUE   <NA>
      3   comfort_A       beta_3   comfort           A         TRUE      TRUE   <NA>
      4   comfort_B       beta_4   comfort           B         TRUE      TRUE   <NA>
      5   comfort_C       beta_5   comfort           C         TRUE      TRUE   <NA>
      6       price       beta_6     price        <NA>         TRUE     FALSE     cn
      7    income_B       beta_7    income           B        FALSE      TRUE     cn
      8    income_C       beta_8    income           C        FALSE      TRUE     cn

