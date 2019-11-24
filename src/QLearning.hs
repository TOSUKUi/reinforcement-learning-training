module QLearning
    ( qLearning
    )
where

import qualified Data.Map                      as M
import           System.Random
import           Data.Random
import           Debug.Trace


data Action = UP | DOWN | LEFT | RIGHT deriving (Show, Ord, Eq)

type QMap = M.Map QIndex Double
type State = (Int, Int)
type CakePosition = (Int, Int)
type QIndex = (State, Action)
type Height = Int
type Width = Int
type Field = (Height, Width)
type SimulationLog = [(State, Action)]
type SimulationResult = (SimulationLog, QMap)


qLearning :: IO ()
qLearning = do
    let states = [ (a, b) | a <- [0 .. 4], b <- [0 .. 4] ]
    let actions      = [UP, DOWN, LEFT, RIGHT]
    let field        = (4, 4)
    let qMap         = initQMap actions states
    let epsilon      = 0.1
    let cakePosition = (3, 4)
    randomChoices    <- randomListi 100 (0, 3)
    adventureRandoms <- randomListd 100 (0, 1)
    let simulationLog = []
    putStrLn $ show $ episode actions
                              (0, 0)
                              field
                              qMap
                              epsilon
                              cakePosition
                              randomChoices
                              adventureRandoms
                              simulationLog


randomListi :: Int -> (Int, Int) -> IO ([Int])
randomListi 0 (a, b) = return []
randomListi n (a, b) = do
    r  <- randomRIO (a, b)
    rs <- randomListi (n - 1) (a, b)
    return (r : rs)

randomListd :: Int -> (Double, Double) -> IO ([Double])
randomListd 0 (a, b) = return []
randomListd n (a, b) = do
    r  <- randomRIO (a, b)
    rs <- randomListd (n - 1) (a, b)
    return (r : rs)


episode
    :: [Action]
    -> State
    -> Field
    -> QMap
    -> Double
    -> CakePosition
    -> [Int]
    -> [Double]
    -> SimulationLog
    -> SimulationResult
episode actions state field qMap epsilon cakePosition (randomChoice : randomChoices) (adventureRandom : adventureRandoms) simulationLog
    = do
        let fieldmin     = (0, 0)
        let legalActions = legalAction state actions fieldmin field
        let adventure    = (1 - epsilon) < adventureRandom
        let randomChoiceWithin = if randomChoice < length legalActions
                then randomChoice
                else (length legalActions) - 1
        let nextAction = epsilonGreedy randomChoiceWithin
                                       adventure
                                       (maxAction state qMap legalActions)
                                       legalActions
        let next = nextState state nextAction
        let nextQMap = updateQFunc (reward state nextAction cakePosition)
                                   (state, nextAction)
                                   qMap
        episode actions
                next
                field
                nextQMap
                epsilon
                cakePosition
                randomChoices
                adventureRandoms
                ((state, nextAction) : simulationLog)
episode actions state field qMap epsilon cakePosition [] [] simulationLog =
    (simulationLog, qMap)

initQMap :: [Action] -> [State] -> QMap
initQMap actions states =
    M.fromList [ ((s, a), 0) | s <- states, a <- actions ]

qFunc :: State -> QMap -> Action -> Maybe Double
qFunc state qmap action = M.lookup (state, action) qmap
reward state action cakePosition | nextCakeDistance == 0            = 10
                                 | cakeDistance > nextCakeDistance  = -1
                                 | cakeDistance == nextCakeDistance = 0
                                 | cakeDistance < nextCakeDistance  = 1  where
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
legalAction state actions fieldmin fieldmax = traceShow result $ result
  where
    result =
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
    | adventure = traceShow (randomChoice, actions) $ actions !! randomChoice
    | otherwise = maxAction

updateQFunc :: Double -> QIndex -> QMap -> QMap
updateQFunc reward qindex qmap =
    M.update (Just . ((reward :: Double) -)) qindex qmap


maxfa :: (Ord b) => (a -> b) -> [a] -> a
maxfa f []       = error "empty list"
maxfa f (a : as) = iter a as
  where
    iter a [] = a
    iter a (y : ys) | f a > f y = iter a ys
                    | otherwise = iter y ys




maxfb :: (Ord b) => (a -> b) -> [a] -> b
maxfb f as = maximum $ map f as
