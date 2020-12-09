module Train (main) where

import Prelude
import Lib
import Submission2
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (unfoldr)
import Data.List
import Data.Maybe
import Text.Printf
import Control.DeepSeq
import GHC.Generics

type Gain = Double

computeGain :: GameState -> AIState -> PlanetId -> [Double] -> (Gain, Log, AIState)
computeGain gs ai pid params
  | isFirstTime = (gain, [], ai { planetRanking = Just rank, gain = gain })
  | otherwise   = (gain, [], ai { gain += gain })
      where gain = (params !! 0) * pagerank + (params !! 1) * turns + (params !! 2) * damaged + (params !! 3) * compromised
            pagerank :: Double
            pagerank
              | planetRanking ai == Nothing = rank M.! (fromJust target)
              | otherwise = (fromJust (planetRanking ai)) M.! (fromJust target)
            (Path turns _) = (fromJust (shortestPath pid target gs))
            damaged :: Int
            damaged
              | destOwner == Player2 && destShips < srcShips = destShips             --reward here
              | destOwner == Player2 && destShips >= srcShips = srcShips - destShips --punishment here
              | otherwise = 0
            compromised :: Int
            compromised
              | destOwner /= Palyer1 && destShips < srcShips = destShips --punishment here
              | destOwner /= Palyer1 && destShips >= srcShips = srcShips - destShips --punishment here
              | otherwise = 0
            rank   = planetRank gs
            target = if planetRanking ai == Nothing then getTarget gs rank else getTarget gs (fromJust (planetRanking ai))
            (Planet destOwner destShips destGrowth) = lookupPlanet (fromJust target) gs
            (Planet _ srcShips srcGrowth)    = lookupPlanet pid gs

computeDecision :: GameState -> AIState -> [Double] -> ([Order], Log, AIState)
computeDecision gs ai params = s