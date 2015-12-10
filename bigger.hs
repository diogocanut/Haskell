maior :: [Int] -> (Int,Int)
maior []  = (0,0)
maior xs = (bigger xs,pos (bigger xs) xs)

bigger :: [Int] -> Int
bigger []     = 0
bigger (y:ys) = if y > bigger ys then y else bigger ys 

pos :: Int -> [Int] -> Int
pos _ []     = 1
pos x (w:ws) = if x /= w then 1 + pos x ws else pos x []
