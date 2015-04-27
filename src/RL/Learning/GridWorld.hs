
import GHC.IO
import GHC.IORef
import System.IO 
import System.Console.ANSI -- clearScreen
import Text.Printf
import Control.Monad
import Control.Concurrent.Thread.Delay
import Control.Lens  -- replace element
import qualified QLearning as QLearn

--data Cell = Cell { x :: Int, y :: Int } deriving (Show, Eq, Ord)
--data Grid = Grid [Cell] deriving (Show)

cols = 10
rows = 7
sp   = '.'
gridWorld = [ sp, sp, sp, sp, sp, sp, sp, sp, sp, sp
            , sp, sp, sp, sp, sp, sp, sp, sp, sp, sp
            , sp, sp, sp, sp, sp, sp, sp, sp, sp, sp
            , sp, sp, sp, sp, sp, sp, sp, sp, sp, sp
            , sp, sp, sp, sp, sp, sp, sp, sp, sp, sp
            , sp, sp, sp, sp, sp, sp, sp, sp, sp, sp
            , sp, sp, sp, sp, sp, sp, sp, sp, sp, sp]

goal  = 9
start = 60
nDelay = 500000

-- c(columns), r(rows), cur(current), gl(goal)
data World = World{state :: [Char], -- state of the world
                      c  :: Int,    -- columns
                      r  :: Int,    -- rows
                    cur  :: Int,    -- current pose of agent
                     gl  :: Int}    -- goal pose
                   deriving (Show, Eq, Ord)


up    = 1
down  = 2
left  = 3
right = 4
actions = [up, down, left, right]
nactions = 4

testActions = [left, down, up, up, right, right, right, right, right, left, right, right, up, up, up, up, right, right, right]



-- print GridWorld
printGrid [] _    = do putStrLn " "
                       putStrLn " "
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
                    trainEp len
                    train   (reps - 1) len

trainEp 0      = putStrLn "End training"
trainEp length = do printf "%s %d\n" "Start Episode #" length
                    world <- setup gridWorld start goal
                    (qT, p) <- QLearn.qLearn 0.1 0.998 0.9 0.05
                    printGrid (state world) (c world)
                    delay nDelay
                    episode' world (qT, p)
                    putStrLn "End episode"
                    trainEp (length - 1)

-- episode function
episode' w (qT, p)
    | isGoal (cur w)    = do printf "%s %d\n" "REWARD: " (r::Int)
                             putStrLn "Win"
    | not (null xs)     = do printf "%s %s\n" "MOVE: " (strAction a)
                             w <- step w a
                             printGrid (state w) (c w)
                             printf "%s %d\n" "REWARD: " (r::Int)
                             delay nDelay
                             episode' w (qT, p)
    | otherwise         = return ()
                          where r = getReward (cur w)
                                a = QLearn.pickAction (grid w) nactions (eps p) qT


updateWorld xs idx val = return world
                         where world = xs & element idx .~ val


step w a = do t1 <- updateWorld (state w) (cur w) sp
              -- check if is win
              t2  <- updateWorld t1 s' '*'
              return (World{state=t2, c=(c w), r=(r w), cur=s', gl=(gl w)})
              --return (t2,s')
              where s' = getIndex a (cur w)

isGoal s = if s == goal then True else False

getReward s
    | s == goal  = 100
    | otherwise  = -1

getIndex a s
    | a == up    = if s-cols > 0
                      then s-cols else s
    | a == down  = if s+cols < cols*rows - 1
                      then s+cols else s
    | a == left  = if s `mod` cols == 0 
                      then s else s-1
    | a == right = if (s+1) `mod` cols == 0
                      then s else s+1
    | otherwise  = 0 -- error


-- do notation
getTwoChars :: IO (Char, Char)
getTwoChars = do { c1 <- getChar ;
                   c2 <- getChar ;
                   return (c1,c2)
              }

putTwoChars :: (Char,Char) -> IO ()
putTwoChars (c1,c2) = do { putChar c1; putChar c2 }

-- do { x <- e; s } = e >>= \x -> do { s }
--      do { e; s } = e >> do { s }
--         do { e } = e

-- already exists!
--getLine :: IO [Char]
--getLine = do { c <- getChar ;
--               if c == '\n' then
--                 return []
--               else
--                 do { cs <- getLine ;
--                      return (c:cs)
--                 }
--          }



-- Control structures
-- Infinite loop
-- already exists!~
--forever :: IO () -> IO ()
--forever a = a >> forever a

-- repeat n times
repeatN :: Int -> IO a -> IO ()
repeatN 0 a = return ()
repeatN n a = a >> repeatN (n-1) a

-- for loop
for :: [a] -> (a -> IO ()) -> IO ()
for []      fa = return ()
for (n: ns) fa = fa n >> for ns fa
-- ex: printNums = for [1..10] print

-- another for
sequence_ :: [IO a] -> IO ()
sequence_ as = foldr (>>) (return ()) as

-- already exist!
--sequence :: [IO a] -i> IO [a]
--sequence []     = return []
--sequence (a:as) = do { r  <- a;
--                       rs <- sequence as;
--                       return (r:rs) }



-- C program 
-- count ( int n) {
--    int i, v = 0;
--    for (i=1; i <=n; i++) v = v+1;
--    return (v);
-- Haskell (bad technique)
count :: Int -> IO Int
count n = do { r <- newIORef 0 ;
               loop r 1 }
          where
            loop :: IORef Int -> Int -> IO Int
            loop r i | i>n       = readIORef r
                     | otherwise = do { v <- readIORef r ;
                                        writeIORef r (v+1) ;
                                        print v ;
                                        loop r (i+1) }

-- Allocating a global mutable variable
noOfOpenFiles :: IORef Int
noOfOpenFiles = unsafePerformIO (newIORef 0)

-- Emitting trace messages for debugging purposes
trace :: String -> a -> a
trace s x = unsafePerformIO (putStrLn s >> return x)


