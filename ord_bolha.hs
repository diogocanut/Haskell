ordBolha [] = []
ordBolha xs = iterate troca xs !! (length xs - 1)
where troca [x] = [x]
      troca (x:y:zs)
         | x > y = y: troca (x:zs)
         | otherwise = x: troca (y:zs)



iterate f x = x: iterate f (f x)
