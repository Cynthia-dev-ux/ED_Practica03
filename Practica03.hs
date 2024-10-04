
data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud Void = 0 
longitud (Node _ xs) = 1 + longitud xs 


estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void a = False
estaContenido (Node x xs) a = x == a || estaContenido xs a


conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node x xs) = if x == x then xs
                       else Void
