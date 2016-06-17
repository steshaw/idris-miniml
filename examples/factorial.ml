fun fact(n : int) : int is
  if n = 0 then 1
  else n * fact (n - 1);;

fact 10
