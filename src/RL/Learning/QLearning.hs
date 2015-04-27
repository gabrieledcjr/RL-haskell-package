--module RL.Learning.QLearning
module QLearning
( Params
, State
, Action
, qLearn
--, pickAction
--, myGlobalVar
--, createHash
) where

import Control.Monad
import Data.IORef
import System.IO.Unsafe
import System.Random

import qualified Data.HashTable.IO as H


type HashTable k v = H.BasicHashTable k v

-- alpha, gamma, tau, epsilon
-- startEpisode
-- processStep
-- evaluateAction

data Params = Params { alpha :: Float     -- learning rate
                     , gamma :: Float     -- discount factor
                     , tau   :: Float     -- decay
                     , eps   :: Float     -- exploration rate
                     } deriving (Show, Eq, Ord)

type State  = String
type Action = String


data Key = Key State | KeyPair State Action deriving (Eq, Ord, Show, Read)

initV = 0.0

-- Program can only work with known state space
-- Load all state space in a list and convert it to Data.Map

qLearn a g t e = do qT <- createQT
                    return (qT, Params{alpha=a, gamma=g, tau=t, eps=e})

createQT :: IO (HashTable String Double)
createQT = do qT <- H.new
              return qT


pickAction g numA ep qT = do rNum <- randomIO :: IO Float
                             rInt <- randomIO :: IO Int
                             (qT, action) <- pickAction' g numA ep qT rNum rInt
                             return (qT, action)

pickAction' g numA ep qT rNum rInt
    | rNum < ep  = return (qT, action1)
    | otherwise  = do (qT, qVals) <- checkQs g [1..numA] qT []
                      maxQ <- getMax (zip [1..] (reverse qVals)) 0 []
                      rInt <- randomIO :: IO Int
                      action2 <- bestAction maxQ (length maxQ) rInt
                      return (qT, action2)
                      where action1 = (rInt `mod` numA) + 1


bestAction maxQ len rInt = return (head action)
                           where maxQz = zip [1..] maxQ
                                 ranIdx = (rInt `mod` len) + 1
                                 action = [a | (idx,a) <- maxQz, idx==ranIdx]


checkQs _ []     qT xs = return (qT, xs)
checkQs s (a:as) qT xs = do (qT, val) <- getQ s a qT
                            checkQs s as qT (val:xs)

getQ s a qT = do qVal <- H.lookup qT key
                 (qT, qVal') <- checkQT qVal qT
                 return (qT, qVal')
                 where key = (show a) ++ s
                       checkQT val qT = case val of
                                         Nothing -> do H.insert qT key initV
                                                       return (qT, initV)
                                         Just x  -> return (qT, x)


getMax []     _   maxQ = return maxQ
getMax (x:xs) max maxQ
    | maxQ == [] ||
      max  == val   = getMax xs val (idx:maxQ)
    | max  <  val   = getMax xs val (idx:[])
    | otherwise     = getMax xs max maxQ
                      where val = snd x
                            idx = fst x

-- s1(current state), a1(current action), 
-- r(reward), s2(next state)
--learn s1 a1 r s2 = do 
