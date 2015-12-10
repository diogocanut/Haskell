bubbleSort ::[Int] -> Int -> [Int]
bubbleSort _ 0  = []
bubbleSort xs l = bubbleSort (troca xs) (l-1) 
      

troca :: [Int] -> [Int]
troca []       = []
troca (a:b:as) = if b>a then b:a:troca as else troca(a:b:as)
