module Env where

import qualified Data.Map                      as M
import           System.Random
import           Data.Random

main :: IO ()
main = do

    let states = ()
    let qTable = initQMap State
    putStrLn "1"


data Action = UP | DOWN | LEFT | RIGHT deriving (Show, Ord, Eq)

type QMap = M.Map QIndex Int
type State = (Int, Int)
type CakePosition = (Int, Int)
type QIndex = (State, Action)
type Height = Int
type Width = Int
type Field = (Height, Width)


step :: [Action] -> State -> QMap -> Double -> State
step actions state qmap random = do
    let legalActions = legalAction actions
    let randomChoice <- randomRIO 0 $ length legalAction
    let adventure = (1 - epsilon





initQMap :: [Action] -> [State] -> QMap
initQMap actions states =
    M.fromList [ ((s, a), 0) | s <- states, a <- actions ]

qFunc :: State -> QMap -> Action -> Maybe Int
qFunc state qmap action = M.lookup (state, action) qmap

reward :: State -> Action -> CakePosition -> Int

reward state action cakePosition | nextCakeDistance == 0            = 10
                                 | cakeDistance > nextCakeDistance  = -1
                                 | cakeDistance == nextCakeDistance = 0
                                 | cakeDistance < nextCakeDistance  = 1
  where
    cakeDistance     = manlen state cakePosition
    nextCakeDistance = manlen (nextState state action) cakePosition



manlen :: (Int, Int) -> (Int, Int) -> Int
manlen p1 p2 = (abs $ (fst p1) - (fst p2)) + (abs $ (snd p1) - (snd p2))



nextState :: State -> Action -> State
nextState state action | action == UP    = (fst state, (snd state) + 1)
                       | action == DOWN  = (fst state, (snd state) - 1)
                       | action == RIGHT = ((fst state) + 1, snd state)
                       | action == LEFT  = ((fst state) - 1, snd state)


legalAction :: State -> [Action] -> Field -> Field -> [Action]
legalAction state actions fieldmin fieldmax =
    filter ((legalState fieldmin fieldmax) . (nextState state)) actions

legalState :: Field -> Field -> State -> Bool
legalState state fieldmin fieldmax = and
    [ (fst fieldmin) < (fst state)
    , (fst state) < (fst fieldmax)
    , (snd fieldmin) < (snd state)
    , (snd state) < (snd fieldmax)
    ]


maxAction :: State -> QMap -> [Action] -> Action
maxAction state qmap actions = maxfa (qFunc state qmap) actions

epsilonGreedy :: Int -> Bool -> Action -> [Action] -> Action
epsilonGreedy randomChoice adventure maxAction actions
    | adventure = actions !! randomChoice
    | otherwise = maxAction

updateQFunc :: Int -> QIndex -> QMap -> QMap
updateQFunc updateAmount qindex qmap =
    M.update (Just . (+ updateAmount)) qindex qmap


maxfa :: (Ord b) => (a -> b) -> [a] -> a
maxfa f (a : as) | f a > (maxfb f as) = a
                 | otherwise          = maxfa f as

maxfb :: (Ord b) => (a -> b) -> [a] -> b
maxfb f as = maximum $ map f as
