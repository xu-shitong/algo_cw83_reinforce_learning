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

computeGain :: GameState -> AIState -> PlanetId -> PlanetId -> [Double] -> (Gain, Log, AIState)
computeGain gs ai srcpid destpid params
  | isFirstTime = (gain, ["Gain computed"], ai { planetRanking = Just rank } )
  | otherwise   = (gain, ["Gain computed"], ai )
      where gain = (params !! 0) * pagerank + (params !! 1) * turns + (params !! 2) * damaged + (params !! 3) * compromised
            pagerank :: Double
            pagerank
              | planetRanking ai == Nothing = rank M.! destpid
              | otherwise = (fromJust (planetRanking ai)) M.! destpid
            (Path turns _) = (fromJust (shortestPath srcpid destpid gs))
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
            target = lookupPlanet destpid gs
            (Planet destOwner destShips destGrowth) = lookupPlanet destpid gs
            (Planet _ srcShips srcGrowth) = lookupPlanet srcpid gs

computeDecision :: GameState -> AIState -> [Double] -> ([Order], Log, AIState)
computeDecision gs ai params = 
  where gains = [ [computeGain gs ai ours destpid params | destpid <- map target (edgesFrom gs ours) ] | ours <- ourPlanets ]
        ourPlanets = filter (\x -> ourPlanets (lookupPlanet x gs)) (vertices gs)