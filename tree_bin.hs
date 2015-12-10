data ArvBinInt = Nil | NoInt Int ArvBinInt ArvBinInt deriving Show
arvDados::ArvBinInt
arvDados = NoInt 7 
 (NoInt 5  (NoInt 2 Nil Nil)  (NoInt 6 Nil Nil))
 (NoInt 13 (NoInt 9 Nil Nil)  (NoInt 20 Nil (NoInt 23 Nil Nil)))

vazia :: ArvBinInt -> Bool
vazia Nil = True
vazia _   = False

sub_esquerda :: ArvBinInt -> ArvBinInt
sub_esquerda Nil             = Nil
sub_esquerda (NoInt z x y) = x

insere_ord :: Int -> ArvBinInt -> ArvBinInt
insere_ord x Nil = (NoInt x Nil Nil)
insere_ord x (NoInt z w y)     = if x < z then (NoInt z (insere_ord z w) y) else (NoInt z w (insere_ord z y))



deletemax :: ArvBinInt -> (Int,ArvBinInt)
deletemax (NoInt y t1 Nil) = (y,t1)
deletemax (NoInt y t1 t2)  = (z, NoInt y t1 tz)
      where (z,tz) = deletemax t2
