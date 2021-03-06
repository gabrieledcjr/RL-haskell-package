--module RL.Learning.TDControl
module TDControl
( Params
, qLearn
, pickAction
, learn
, HashTable
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

type HashTable k v = H.BasicHashTable k v


initV  = 0.0
params = Params {alpha=0.2, gamma=0.99, tau=0.9, eps=0.1}

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
{--
learn s1 a1 r s2 qT numA 0  = do (qT, qVals)  <- checkQs s2 numA qT
                                 print (zip [1..] qVals)
		                 (_ , maxQ) <- getMax (zip [1..] qVals) 0
                                 let gm = (gamma params)
                                 qT           <- learnQ s1 a1 (r+gm*maxQ) qT
				 return qT
learn s1 a1 r s2 qT numA a2 = do (qT, qVals)  <- checkQs s2 numA qT
			         let qNext = head [ qVal | qVal <- qVals, i <- [1..], i == a2 ]
                                 print (zip [1..] qVals)
			         let gm = (gamma params)
                                 qT           <- learnQ s1 a1 (r+gm*qNext) qT
                                 return qT

--}


learn s1 a1 r s2 qT numA a2 = do (qT, qVals)  <- checkQs s2 numA qT
                                 print (zip [1..] qVals)
                                 let qVal = determineQ qVals a2
                                 let gm = (gamma params)
                                 qT           <- learnQ s1 a1 (r+gm*qVal) qT
				 let key = (show a1) ++ s1
			         print key
                                 return qT
                              where determineQ qs a2
			            	| a2 <= 0   = snd $ getMax (zip [1..] qs) 0
                                        | otherwise = head [qVal | qVal <- qs, i <- [1..], i == a2]



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
                                                      return qT

pickAction g numA qT = do s <- newStdGen
	                  let rNum = head (randoms s :: [Double])
		          let rInt = head (randoms s :: [Int])
                          (qT, action) <- pickAction' g numA qT rNum rInt
                          return (qT, action)

pickAction' g numA qT rNum rInt
    | rNum < ep  = do print "Exploring"
		      let action1 = (rInt `mod` numA) + 1 
                      return (qT, action1)
    | otherwise  = do (qT, qVals) <- checkQs g numA qT
                      let (maxA,_) = getMax (zip [1..] qVals) 0
                      rInt        <- randomIO :: IO Int
                      action2     <- bestAction maxA (length maxA) rInt
                      print "Exploiting"
                      return (qT, action2)
                   where ep = (eps params)


bestAction maxA len rInt = do let maxAz  = zip [1..] maxA
                              let ranIdx = (rInt `mod` len) + 1
                              let action = [a | (idx,a) <- maxAz, idx==ranIdx]  
                              return (head action)

checkQs  s numA qT    = checkQs' s numA qT []
checkQs' _ 0    qT xs = return (qT, xs)
checkQs' s numA qT xs = do (qT, val) <- getQ s numA qT
                           checkQs' s (numA-1) qT (val:xs)

getQ s a qT = do qVal        <- H.lookup qT key
                 (qT, qVal') <- checkQT qVal qT
                 return (qT, qVal')
                 where key = (show a) ++ s
                       checkQT val qT = case val of
                                         Nothing -> do H.insert qT key initV
                                                       return (qT, initV)
                                         Just x  -> return (qT, x)

getMax  xs     max      = getMax' xs max []
getMax' []     max maxA = (maxA, max)
getMax' (x:xs) max maxA
    | maxA == [] ||
      max  == val   = getMax' xs val (idx:maxA)
    | max  <  val   = getMax' xs val (idx:[])
    | otherwise     = getMax' xs max maxA
                      where val = snd x
                            idx = fst x

