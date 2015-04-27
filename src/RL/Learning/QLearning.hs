--module RL.Learning.QLearning
module QLearning
( Params
, State
, Action
--, myGlobalVar
--, createHash
) where

import Control.Monad
import Data.IORef
import System.IO.Unsafe

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

-- Program can only work with known state space
-- Load all state space in a list and convert it to Data.Map


--getQ s a qT = do 

-- s1(current state), a1(current action), 
-- r(reward), s2(next state)
--learn s1 a1 r s2 = do 
