
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
data World = World {state :: [Char], -- state of the world
                   ,c     :: Int,    -- columns
                   ,r     :: Int,    -- rows
                   ,cur   :: Int,    -- current pose of agent
                   ,gl    :: Int}    -- goal pose
                   deriving (Show, Eq, Ord)


up       = 1
down     = 2
left     = 3
right    = 4
actions  = [up, down, left, right]
nactions = 4

limit = 5000

-- print GridWorld
printGrid [] _    = putStrLn " "
printGrid xs cols = do putStrLn  (take cols xs)
                       printGrid (drop cols xs) cols

strAction a
    | a == 1    = "UP"
    | a == 2    = "DOWN"
    | a == 3    = "LEFT"
    | a == 4    = "RIGHT"
    | otherwise = "Invalid!"


setup xs s g = do t1 <- updateWorld xs s '*'
                  t2 <- updateWorld t1 g '+'
                  --return t2
                  return (World{state = t2, c = cols, r = rows, cur = s, gl = g})

-- train function
train 0    _   = putStrLn "Program ends"
train reps len = do print   ("REPS", reps)
		    qT <- AI.qLearn
                    trainEp len qT []
                    train   (reps-1) len

trainEp 0      _  scores = do print scores
                              putStrLn "End training"
trainEp length qT scores = do printf "%s %d\n" "Start Episode #" length
                              world <- setup gridWorld start goal
                              printGrid (state world) (c world)
                              delay nDelay
                              (qT, score) <- episode' world qT (state world) 0 0 0
                              printf "%s %f\n" "Score: " score
                              delay (nDelay+800000)
                              putStrLn "End episode"
                              list <- H.toList qT
                              print list
                              trainEp (length-1) qT (score:scores)

-- episode function
episode' w qT lState lAction r score
    | isGoal (cur w)    = do printf "%s %f\n" "REWARD: " (r::Double)
                             putStrLn "Win"
			     (qT, a) <- AI.pickAction (state w) nactions qT
			     qT <- AI.learn lState lAction r (state w) qT nactions 0
                             return (qT, score+r)
--    | limit   == 0      = return (qT, score+r)
    | lAction == 0      = do (qT, a) <- AI.pickAction lState nactions qT
                             printf "%s %s\n" "MOVE: " (strAction a)
                             w       <- step w a
                             printGrid (state w) (c w)
                             printf "%s %f\n" "REWARD: " (r::Double)
                             episode' w qT lState a r (r+score)
    | otherwise         = do (qT, a) <- AI.pickAction (state w) nactions qT
			     qT      <- AI.learn lState lAction r (state w) qT nactions 0
                             printf "%s %s\n" "MOVE: " (strAction a)
		             let lState = (state w)
                             w       <- step w a
                             printGrid (state w) (c w)
                             printf "%s %f\n" "REWARD: " (r::Double)
                             delay nDelay
                             episode' w qT lState a r (r+score)
                          where r = getReward (cur w)


updateWorld xs idx val = return world
                         where world = xs & element idx .~ val


step w a = do t1 <- updateWorld (state w) (cur w) sp
              t2 <- updateWorld t1 s' '*'
              return (World{state=t2, c=(c w), r=(r w), cur=s', gl=(gl w)})
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



-- Emitting trace messages for debugging purposes
trace :: String -> a -> a
trace s x = unsafePerformIO (putStrLn s >> return x)


