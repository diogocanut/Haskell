newtype Set a = SetI [a] deriving Show
-- Interseção entre conjuntos



inter:: Ord a => Set a ->Set a ->Set a
inter (SetI xs) (SetI ys) = SetI (ints xs ys)
ints:: Ord a =>[a]->[a]->[a]
ints [] ys = []
ints xs [] = []
ints (x:xs) (y:ys) 
| x < y  = ints xs (y:ys)
| x == y = x: ints xs ys
| otherwise = ints (x:xs) ys



-- Mapeamento e Filtragem para conjuntos
mapSet::Ord b=> (a->b)-> Set a-> Set b
mapSet f (SetI xs) = makeSet (map f xs)
filterSet::(a->Bool)-> Set a-> Set a
filterSet p (SetI xs) = SetI (filter p xs)
makeSet::Ord a=> [a]->Set a
makeSet xs = SetI (remDup (qsort xs))
remDup::Ord a=>[a]->[a]
remDup [] = []
remDup [x] = [x]
remDup (x:y:xs) 
   | x<y = x: remDup (y:xs)
   | otherwise = remDup (y:xs)



-- Função Quicksort 
qsort [] = []
qsort (x:xs) = qsort menores ++ [x] ++ qsort maiores
   where menores = [h|h<-xs, h<x]
         maiores = [h|h<-xs, h>=x]


-- Função diferença entre conjuntos
diff::Ord a => Set a -> Set a -> Set a
diff (SetI xs) (SetI ys) = SetI (remove (ints xs ys) xs)
remove::Ord a=>[a]->[a]->[a]
remove xs [] = []
remove [] ys = ys
remove (x:xs) (y:ys) 
   | x==y = remove xs ys
   | otherwise = y:remove (x:xs) ys
