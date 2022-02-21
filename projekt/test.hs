import Control.Exception
import Prelude hiding(catch)
import Data.Binary (Binary(putList))
import qualified Table as Ta
import System.IO
import System.Random

randString :: IO String
randString = fmap  (take 30 . randomRs ('a','z')) newStdGen