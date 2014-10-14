import System.IO
import System.Environment
import System.IO.Error
import Data.List
import Control.Monad
import Numeric
import System.Random
import Data.Fixed

data node = node { vists :: Int,
                   reward :: Int,
                   obstacle :: Char,
                   distractor :: Float,
                 } deriving (Show)

