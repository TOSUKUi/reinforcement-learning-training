module Env where

import qualified Data.Map                      as M
import           System.Random
import           Data.Random

main :: IO ()
main = do
    let states = State
    let qTable = initQMap State
    putStrLn "1"


data Action = UP | DOWN | LEFT | RIGHT deriving (Show, Ord, Eq)

type QMap = M.Map QIndex Int
type State = (Int, Int)
type QIndex = (State, Action)
type Height = Int
type Width = Int
type Field = (Height, Width)


initQMap :: [Action] -> [State] -> QMap
initQMap actions states =
    M.fromList [ ((s, a), 0) | s <- states, a <- actions ]

qFunc :: State -> QMap -> Action -> Maybe Int
qFunc state qmap action = M.lookup (state, action) qmap

reward :: State -> Action -> Int
reward _ = 1


legalAction :: State -> Field -> [Action]
legalAction _ _ = [UP]

maxAction :: State -> QMap -> [Action] -> Action
maxAction state qmap actions = maxfa (qFunc state qmap) actions

epsilonGreedy :: Int -> Bool -> Action -> [Action] -> Action
epsilonGreedy randomChoice adventure maxAction actions
    | adventure = actions !! randomChoice
    | otherwise = maxAction

updateQFunc :: Int -> QMap -> QMap
updateQFunc _ _ = M

maxfa :: (Ord b) => (a -> b) -> [a] -> a
maxfa f (a : as) | f a > (maxfb f as) = a
                 | otherwise          = maxfa f as

maxfb :: (Ord b) => (a -> b) -> [a] -> b
maxfb f as = maximum $ map f as
