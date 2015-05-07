module GridWorld2
( World(grid, c, r)
, State(sWorld,sQTab,sOffset)
, emptyState
, isGoal'
, isAgent
, trainEp
) where

import GHC.IO
import GHC.IORef
import System.IO 
import System.Console.ANSI -- clearScreen
import Text.Printf
import Control.Monad
import Control.Concurrent.Thread.Delay
import Control.Lens  -- replace element
import qualified TDControl as AI
import qualified Data.HashTable.IO as H
import Control.Concurrent.MVar as MV

--data Cell = Cell { x :: Int, y :: Int } deriving (Show, Eq, Ord)
--data Grid = Grid [Cell] deriving (Show)

sp   = '.'

--cols = 10
--rows = 7
--goal  = 9
--start = 60

--cols = 10
--rows = 10
--goal = 9
--start = 90

cols = 5
rows = 5 
goal  = 4
start = 20

--nDelay = 250000
nDelay = 10000
--nDelay = 0

-- creates Gridworld
gridWorld = [ sp | repeat <- [1..(cols*rows)] ]

-- c(columns), r(rows), cur(current), gl(goal)
data World = World {grid :: [Char] -- state of the world
                   ,c    :: Int    -- columns
                   ,r    :: Int    -- rows
                   ,cur  :: Int    -- current pose of agent
                   ,gl   :: Int}   -- goal pose
                   deriving (Show, Eq, Ord)

data State = State {sWorld :: World
                   ,sQTab  :: AI.HashTable String Double
                   ,sOffset :: Double}

up       = 1
down     = 2
left     = 3
right    = 4
actions  = [up, down, left, right]
nactions = 4

limit = 5000

-- print GridWorld
printGrid [] _    = putStrLn " "
printGrid xs cols = do 
    putStrLn  (take cols xs)
    printGrid (drop cols xs) cols

strAction a
    | a == 1    = "UP"
    | a == 2    = "DOWN"
    | a == 3    = "LEFT"
    | a == 4    = "RIGHT"
    | otherwise = "Invalid!"


setup xs s g = do 
    t1 <- updateWorld xs s '*'
    t2 <- updateWorld t1 g '+'
    return t2

emptyState qT = do
    g <- setup gridWorld start goal
    let world = World {grid = g
                      ,c    = cols
                      ,r    = rows
                      ,cur  = start
                      ,gl   = goal}
    return $ State {sWorld = world
                   ,sQTab  = qT
                   ,sOffset = 0}

isGoal' grid (x,y) = if (grid!!index) == '+' then True else False
                     where index = (y*cols) + x
isAgent grid (x,y) = if (grid!!index) == '*' then True else False
                     where index = (y*cols) + x
    


trainEp length state = trainEp' length state []
trainEp' 0      _  scores = do 
    print scores
    putStrLn "End training"

trainEp' length state scores = do 
    world <- liftM sWorld $ MV.readMVar state
    score <- episode state
    trainEp' (length-1) state (score:scores)

-- episode function
episode state = do
    s <- MV.readMVar state
    let world = sWorld s
        qT    = sQTab  s

    printf "%s\n" "Start Episode"
    printGrid (grid world) (c world)

    (qT',score) <- episode' world qT (grid world) 0 0 0
    MV.modifyMVar_ state (\s -> return s{sQTab = qT'})

    printf "%s %f\n" " Score: " score
    delay (nDelay+800000)
    list <- H.toList qT'
    print list
    putStrLn "End episode"

    return score
   
episode' w qT lState lAction r score
    | isGoal (cur w) = do 
          printf "%s %f\n" "REWARD: " (r::Double)
          putStrLn "Win"
	  (qT, a) <- AI.pickAction (grid w) nactions qT
          qT      <- AI.learn lState lAction r (grid w) qT nactions 0
          return (qT, score+r)

--    | limit   == 0      = return (qT, score+r)

    | lAction == 0   = do 
          (qT, a) <- AI.pickAction lState nactions qT
          w       <- step w a
          printf "%s %s\n" "MOVE: " (strAction a)
          printGrid (grid w) (c w)
          printf "%s %f\n" "REWARD: " (r::Double)
          episode' w qT lState a r (r+score)

    | otherwise      = do 
          (qT, a) <- AI.pickAction (grid w) nactions qT
	  qT      <- AI.learn lState lAction r (grid w) qT nactions 0
          let lState = (grid w)
          w       <- step w a
          printf "%s %s\n" "MOVE: " (strAction a)
          printGrid (grid w) (c w)
          printf "%s %f\n" "REWARD: " (r::Double)
          delay nDelay
          episode' w qT lState a r (r+score)

    where r = getReward (cur w)


updateWorld xs idx val = return world
                         where world = xs & element idx .~ val


step w a = do 
    t1 <- updateWorld (grid w) (cur w) sp
    t2 <- updateWorld t1 s' '*'
    return (World{grid=t2, c=(c w), r=(r w), cur=s', gl=(gl w)})
    where s' = getIndex a (cur w)

isGoal s = if s == goal then True else False

getReward s
    | s == goal  = 100.0 :: Double
    | otherwise  = -1.0 :: Double

getIndex a s
    | a == up    = if s-cols >= 0
                      then s-cols else s
    | a == down  = if s+cols <= cols*rows - 1
                      then s+cols else s
    | a == left  = if s `mod` cols == 0 
                      then s else s-1
    | a == right = if (s+1) `mod` cols == 0
                      then s else s+1
    | otherwise  = 0 -- error



-- train function
train 0    _   = putStrLn "Program ends"
train reps len = do 

    qT <- AI.qLearn
    state <- MV.newMVar =<< (emptyState qT)

    trainEp len state
    train   (reps-1) len
