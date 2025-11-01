let rec gcd n m =
  match m with 0 -> n | _ -> if m > n then gcd m n else gcd (n mod m) m
