data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud Void = 0 
longitud (Node _ xs) = 1 + longitud xs 

estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void a = False
estaContenido (Node x xs) a = x == a || estaContenido xs a

convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs) 

convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node x xs) = x : convertirALista xs

conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node x xs) = if estaContenido xs x then conjunto xs
                       else Node x (conjunto xs)

eliminarIndice :: List a -> Int -> List a
eliminarIndice Void x = Void
eliminarIndice (Node x xs) a = if a<0 || a >= longitud (Node x xs) then error "Indice fuera del rango permitido"
                               else if a==0 then xs
                               else Node x (eliminarIndice xs (a - 1))

insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void y z = (Node z Void) 
insertarIndice (Node x xs) y z = if y<0 || y >= longitud (Node x xs) then error "Indice fuera del rango permitido"
                                 else if y==0 then Node z(Node x xs)
                                 else Node x (insertarIndice xs (y - 1) z) 

recorrerLista :: List a -> Int -> List a
recorrerLista Void x = Void
recorrerLista (Node x xs) y = if y==0 then (Node x xs)
                              else recorrerLista (insertarIndice xs(longitud xs) x) (y-1)
