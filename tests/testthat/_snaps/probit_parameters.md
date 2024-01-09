# probit parameter can be created

    Code
      print(x)
    Output
      Parameter:
      C : 1
      alpha : 4 x 1 matrix of doubles 
            [,1]
      [1,] -1.98
      [2,]  0.58
      [3,] -2.64
      [4,]  5.04
      
      b : 1.04
      Omega : 0.7
      Sigma : 3 x 3 matrix of doubles 
           [,1] [,2] [,3]
      [1,]    1    1    1
      [2,]    1 3.32 0.53
      [3,]    1 0.53 4.41
      
      Sigma_diff : 2 x 2 matrix of doubles 
            [,1]  [,2]
      [1,]  2.32 -0.47
      [2,] -0.47  3.41
      
      diff_alt : 1
      beta : 1 x 100 matrix of doubles 
           [,1] [,2] [,3] ... [,100]
      [1,]  2.3 1.37 0.52 ...   2.45
      
      z : double vector of length 100 
      1 1 1 ... 1

---

    Code
      print(x, "beta")
    Output
      Parameter:
      beta : 1 x 100 matrix of doubles 
           [,1] [,2] [,3] ... [,100]
      [1,]  2.3 1.37 0.52 ...   2.45
      

---

    Code
      print(x)
    Output
      Parameter:
      C : 1
      b : 2 x 1 matrix of doubles 
            [,1]
      [1,] -1.98
      [2,]  0.58
      
      Omega : 4 x 1 matrix of doubles 
            [,1]
      [1,]   1.3
      [2,] -1.76
      [3,] -1.76
      [4,]  5.29
      
      Sigma : 0.57
      beta : 2 x 10 matrix of doubles 
            [,1]  [,2]  [,3] ... [,10]
      [1,] -1.14 -2.33 -1.54 ... -2.05
      [2,]  0.43  3.63 -1.08 ...   0.4
      
      z : double vector of length 10 
      1 1 1 ... 1
      d : -1.47

---

    Code
      print(x, "d")
    Output
      Parameter:
      d : -1.47
