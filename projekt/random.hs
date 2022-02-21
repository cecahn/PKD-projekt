import System.Random 
import Control.Monad

Control.Monad Data.Char> mapM (\x -> chr (ord x + 1)) (replicateM 10 (randomRIO ('a','z')))
