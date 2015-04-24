module RL.Learning.QLearning
( Params
, State
, Action
) where


import Control.Monad
import qualified Data.HashTable.IO as H
import qualified Data.Map as Map


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

type HashTable k v = H.BasicHashTable k v

data Key = Key State | KeyPair State Action deriving (Eq, Ord, Show, Read)


test :: IO (HashTable String Float)
test = do
        ht <- H.new
        H.insert ht "Test" 12
        value <- H.lookup ht "Test"
        putStrLn . show $ value
        return ht

myMap = Map.empty :: Map.Map String Float

myMap' = Map.insert "Foo" 12.12 myMap

-- Program can only work with known state space
-- Load all state space in a list and convert it to Data.Map



