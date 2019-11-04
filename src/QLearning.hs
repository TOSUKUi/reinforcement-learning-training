module Env where

import qualified Data.Map                      as M
import System.Random
import Data.Random

main :: IO ()
main = do
    let states = State
    let qTable = initQMap State
    putStrLn "1"


data Action = UP | DOWN | LEFT | RIGHT deriving (Show)

type QMap = M.Map String Int
type State = (Int, Int)
type Height = Int
type Width = Int
type Field = (Height, Width)


initQMap :: Int -> Int -> QMap
initQMap _ _ = M

qFunc :: State -> QMap -> Action -> Int
qFunc _ _ _ = 1

reward :: State -> Action -> Int
reward _ _ = 1


legalAction :: State -> Field -> [Action]
legalAction _ _ = [UP]

maxAction ::  QMap -> [Action] -> Action
maxAction qmap actions = maxfa (qFunc qmap) actions

epsilonGreedy :: Double -> Action -> [Action] -> Action 
epsilonGreedy epsilon maxAction actions 
    | adventure = randomElement actions
    | otherwise = maxAction
    where adventure =  (epsilon >) <$> (randomRIO (0, 1) :: IO Double)

updateQFunc :: Int -> QMap -> QMap
updateQFunc _ _ = M

maxfa :: (Ord b) => (a -> b) -> [a] -> a
maxfa f (a:as)
    | f a > maxfb as  = a      
    | otherwise = maxfa as
maxfa f a = a

maxfb :: (Ord b) => (a -> b) -> [a] -> b
maxfb f as = maximum $ map f as 
