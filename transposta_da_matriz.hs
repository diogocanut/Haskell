{- adicione a biblioteca de arrays V -} 

import Data.Array


{- declarando a matriz 3x3 V -}
mat = array ((1,1),(3,3)) [((1,1),4), ((1,2),0), ((1,3),3), ((2,1),5), ((2,2),1), ((2,3),4), ((3,1),4), ((3,2),5), ((3,3),6)]




{- função que calcula a determinante -}
deterMat:: (Ix a,Num b) => Array (a,a) b -> Array (a,a) b
deterMat mat3  = array (bounds mat3) [((i,j),mat3!(j,i)) | (i,j) <- range (bounds mat3)]
