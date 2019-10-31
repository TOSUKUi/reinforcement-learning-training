module Env where

import qualified Data.Map                      as M
main :: IO ()
main = do
    let qTable = initQMap 2 2
    putStrLn "1"


data Action = UP | DOWN | LEFT | RIGHT deriving (Show)

type QMap = M.Map
type State = (Int, Int)
type Height = Int
type Width = Int
type Field = (Height, Width)

initQMap :: Int -> Int -> QMap
initQMap _ _ = M

qFunc :: State -> Action -> QMap -> Int
Func _ _ _ = 1

reward :: State -> Action -> Int
reward _ _ = 1

legalAction :: State -> Field -> Action
legalAction _ _ = UP


updateQFunc :: Int -> QMap -> QMap
updateQFunc _ _ = M











