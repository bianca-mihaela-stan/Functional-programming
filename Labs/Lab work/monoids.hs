class Monoid m where
    mempty :: m
    mappend :: m-> m-> m
    mconcat :: [m] -> m
    mconcat = foldr Main.mappend Main.mempty

instance Main.Monoid [a] where
    mempty = []
    mappend = (++)

--de ce Integer nu are o instanta Monoid?
--niciun tip numeric nu are
--de ec? fiindca fiecare tip numeric trebuie sa aiba doar o singura instanta pentry un anumit typeclass

let x = 1 :: Integer
let y = 3 :: Integer 
--asta nu functioneaza: mappend x y
z=mappend (Sum 1) (Sum 5)
z=mappend (Product 1) (Product 5)
