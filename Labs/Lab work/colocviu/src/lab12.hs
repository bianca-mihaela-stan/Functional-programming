import qualified Data.Char     as Char
import           System.Random

newtype MyRandom a = MyRandom { runRandom :: StdGen -> (a,StdGen) }
--StdGen e seed-ul
--   runRandom :: MyRandom a -> StdGen -> (a, StdGen)
--   MyRandom :: (StdGen -> (a, StdGen)) -> MyRandom a

---------------------------------EXERCITIUL 1----------------------------
instance Functor MyRandom where
    -- fmap :: (a -> b) -> MyRandom a -> MyRandom b
    -- f :: (a -> b)
    -- (MyRandom ma) :: MyRandom a  -- (am facut match pe constructor, tineti minte ca putem face asta pe orice constructor)
    -- ma :: StdGen -> (a, StdGen)  -- observati ca sub MyRandom e o functie ma
    fmap f (MyRandom ma) =
        -- ideea aici e ca vrem sa intoarcem un MyRandom, care e o functie peste care punem constructorul MyRandom
        -- remember:
        --   cu $ aplicam MyRandom la ce e in dreapta lui $
        --   sintaxa pentru functii anonime: \arg1 arg2 ... -> corpulunctiei
        -- deci, 'gen' e argumentul de tip StdGen
        MyRandom $ \gen ->
            -- ne uitam la ce avem:
            --   gen :: StdGen
            --   ma :: StdGen -> (a, StdGen)
            --   f :: a -> b
            -- pai, in primul rand putem aplica 'ma' pe 'gen', si sa extragem rezultatul
            let (result, newGen) = ma gen
            -- acum, avem si:
            --   result :: a
            --   newGen :: StdGen
            -- ce vrem sa intoarcem? seed-ul nou si rezultatul transformat cu functia f
             in (f result, newGen)
-- Puteti sa va ganditi ca aici primim doua functii, una a -> b si una care e incapsulata de MyRandom.
-- Noi vrem sa scriem o functie noua, care ruleaza cea de-a doua functie si transforma rezultatul ei
-- folosind-o pe prima. Apoi, o incapsulam si pe ea in MyRandom.

echo :: IO()
echo = do
    {
        line <- getLine;
        if line == "" then
            return()
        else do
        {
            putStrLn (map Char.toUpper line);
            echo
        }
    }