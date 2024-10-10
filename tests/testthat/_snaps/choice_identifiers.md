# choice_identifiers can be printed

    Code
      choice_identifiers(data.frame(deciderID = 1, occasionID = 1))
    Output
       deciderID occasionID
       1         1         

---

    Code
      print(generate_choice_identifiers(N = 1000, Tp = 2), rows = 5)
    Output
       deciderID occasionID
       1         1         
       1         2         
       2         1         
      
    Message
      <1995 rows hidden>
    Output
                           
       1000      1         
       1000      2         

