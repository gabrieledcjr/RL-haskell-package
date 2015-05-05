--module RL.Learning.QLearning
module QLearning
( Params
, State
, Action
, qLearn
, pickAction
, learn
--, myGlobalVar
--, createHash
) where

import Control.Monad
import Data.IORef
import System.IO.Unsafe
import System.Random

import qualified Data.HashTable.IO as H


data Params = Params { alpha :: Double     -- learning rate
                     , gamma :: Double     -- discount factor
                     , tau   :: Double     -- decay
                     , eps   :: Double     -- exploration rate
                     } deriving (Show, Eq, Ord)
data Key = Key State | KeyPair State Action deriving (Eq, Ord, Show, Read)

type HashTable k v = H.BasicHashTable k v
type State  = String
type Action = String


initV  = 0.0
params = Params {alpha=0.2, gamma=0.8, tau=0.9, eps=0.3}

-- Program can only work with known state space
-- Load all state space in a list and convert it to Data.Map

qLearn :: IO (HashTable String Double)
qLearn = do qT <- H.new
            return qT

createQT :: IO (HashTable String Double)
createQT = do qT <- H.new
              return qT

-- s1(current state), a1(current action), 
-- r(reward), s2(next state)
learn s1 a1 r s2 qT numA = do (qT, qVals)  <- checkQs s2 [1..numA] qT []
                              print (reverse qVals)
                              print a1
                              (maxA, maxQ) <- getMax (zip [1..] (reverse qVals)) 0 []
			      let gm = (gamma params)
                              qT           <- learnQ s1 a1 (r+gm*maxQ) qT
                              return qT

learnQ s a val qT = do qVal <- H.lookup qT key
                       qT   <- updateQ qVal qT
                       return qT
                       where key = (show a) ++ s
                             updateQ qVal qT = 
                                     case qVal of
                                      Nothing   -> do H.insert qT key val
                                                      return qT
                                      Just oldv -> do let al = (alpha params)
                                                      H.insert qT key (oldv+al*(val-oldv))
                                                      b <- H.lookup qT key
                                                      return qT

pickAction g numA qT = do rNum         <- randomIO :: IO Double
                          rInt         <- randomIO :: IO Int
                          (qT, action) <- pickAction' g numA qT rNum rInt
                          return (qT, action)

pickAction' g numA qT rNum rInt
    | rNum < ep  = do print "Exploring"
		      let action1 = (rInt `mod` numA) + 1 
                      return (qT, action1)
    | otherwise  = do (qT, qVals) <- checkQs g [1..numA] qT []
                      print (zip [1..] (reverse qVals))
                      (maxA,_)    <- getMax (zip [1..] (reverse qVals)) 0 []
                      print maxA
                      rInt        <- randomIO :: IO Int
                      action2     <- bestAction maxA (length maxA) rInt
                      print "Exploiting"
                      return (qT, action2)
                   where ep = (eps params)


bestAction maxA len rInt = do let maxAz  = zip [1..] maxA
                              let ranIdx = (rInt `mod` len) + 1
                              let action = [a | (idx,a) <- maxAz, idx==ranIdx]  
                              return (head action)

checkQs _ []     qT xs = return (qT, xs)
checkQs s (a:as) qT xs = do (qT, val) <- getQ s a qT
                            checkQs s as qT (val:xs)

getQ s a qT = do qVal        <- H.lookup qT key
                 (qT, qVal') <- checkQT qVal qT
                 return (qT, qVal')
                 where key = (show a) ++ s
                       checkQT val qT = case val of
                                         Nothing -> do H.insert qT key initV
                                                       return (qT, initV)
                                         Just x  -> return (qT, x)


getMax []     max maxA = return (maxA, max)
getMax (x:xs) max maxA
    | maxA == [] ||
      max  == val   = getMax xs val (idx:maxA)
    | max  <  val   = getMax xs val (idx:[])
    | otherwise     = getMax xs max maxA
                      where val = snd x
                            idx = fst x

