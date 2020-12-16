
{-#  LANGUAGE GeneralizedNewtypeDeriving  #-}
{-#  LANGUAGE FlexibleInstances  #-}
{-#  LANGUAGE ScopedTypeVariables  #-}
{-#  LANGUAGE StandaloneDeriving  #-}
{-#  LANGUAGE DeriveGeneric  #-}

module Submission2 where
import Lib

  hiding (example1, example2, example3, lookupPlanet)
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (concatMap, sortBy)
import Data.Maybe ( isNothing, fromJust )
import Data.Set (Set)
import qualified Data.Set as S
import Text.Printf
import Control.DeepSeq
import GHC.Generics

import System.IO  
import Control.Monad

deriving instance (Integral Growth)
deriving instance (Enum Growth)
deriving instance (Real Growth)

data Strategy
  = Pacifist
  | ZergRush
  | PlanetRankRush
  | Skynet
  deriving (Enum, Bounded, Show, Read)

logic :: Strategy -> GameState -> AIState -> ([Order], Log, AIState)
logic strat gs ai
  = let logic' = case strat of
          Pacifist       -> pacifist
          ZergRush       -> zergRush
          PlanetRankRush -> planetRankRush
          Skynet         -> skynet
    in logic' gs ai {turn = turn ai + 1}

data AIState = AIState
  { turn :: Turns
  , rushTarget :: Maybe PlanetId
  , ranks :: PlanetRanks
  , strategyPoints :: PlanetRanks
  , params :: [Double]
  } deriving Generic
 
initialState :: AIState
initialState = AIState
  { turn = 0
  , rushTarget = Nothing
  , ranks = M.empty 
  , strategyPoints = M.empty
  , params = [7, 0, 0.1, 0.1, 50]
  }
  
type Log = [String]

pacifist :: GameState -> AIState -> ([Order], Log, AIState)
pacifist _ ai = ([], ["Do no harm."], ai)

enemyPlanet :: Planet -> Bool
enemyPlanet (Planet (Owned Player2) _ _) = True
enemyPlanet _                            = False

findEnemyPlanet :: GameState -> Maybe PlanetId
findEnemyPlanet st
  | null l    = Nothing
  | otherwise = (Just . fst . head) l
    where l = M.toList (M.filter enemyPlanet ps)
          (GameState ps _ _) = st

send :: WormholeId -> Maybe Ships -> GameState -> [Order]
send wId mShips st
  | owner == Neutral || owner == (Owned Player2) = []
  | mShips == Nothing            = [Order wId totalShips]
  | fromJust mShips > totalShips = [Order wId totalShips]
  | fromJust mShips < 0 = []
  | otherwise = [Order wId (fromJust mShips)]
    where
      Wormhole (Source src) _ _ = lookupWormhole wId st
      planet@(Planet owner totalShips _) = lookupPlanet src st

shortestPath :: PlanetId -> PlanetId -> GameState 
             -> Maybe (Path (WormholeId, Wormhole))
shortestPath src dst st
  = case filter ((== dst) . target) (shortestPaths st src) of
      [] -> Nothing
      (x : _) -> Just x

ourPlanet :: Planet -> Bool
ourPlanet (Planet (Owned Player1) _ _) = True
ourPlanet _ = False

ourPlanets :: GameState -> Planets
ourPlanets (GameState ps _ _) = M.filter ourPlanet ps

lookupWormhole :: WormholeId -> GameState -> Wormhole
lookupWormhole wId (GameState _ wormholes _)
  = wormholes M.! wId

lookupPlanet :: PlanetId -> GameState -> Planet
lookupPlanet pId (GameState planets _ _)
  = planets M.! pId

attackFromAll :: PlanetId -> GameState -> [Order]
attackFromAll targetId gs
  = map pathToOrder paths
    where paths   = map fromJust (filter (\x -> not (isNothing x)) paths')
          paths'  = map (\x -> shortestPath (fst x) targetId gs) (M.toList planets)
          planets = ourPlanets gs
          pathToOrder :: (Path (WormholeId, Wormhole)) -> Order
          pathToOrder (Path _ es) = Order wid mShip
            where (wid, wh)          = last es
                  (Planet _ mShip _) = lookupPlanet (source wh) gs
          

zergRush :: GameState -> AIState 
         -> ([Order], Log, AIState)
zergRush gs ai
  | t == Nothing || o == Player1 = case nextEnemy of  
          Nothing -> ([], [], ai) 
          _       -> zergRush gs ai {rushTarget = nextEnemy }
  | otherwise = (attackFromAll tId gs, ["attacking " ++ (show tId)], ai)
  where 
    nextEnemy = findEnemyPlanet gs
    t = rushTarget ai
    tId = fromJust t
    target@(Planet (Owned o) _ _) = lookupPlanet tId gs 
    
newtype PageRank = PageRank Double
  deriving (Num, Eq, Ord, Fractional)
 
type PageRanks pageId = Map pageId PageRank

instance Show PageRank where
  show (PageRank p) = printf "%.4f" p

initPageRanks :: (Graph g e pageId, Ord pageId) 
              => g -> PageRanks pageId
initPageRanks g = M.fromList [ (p, PageRank (1 / fromIntegral n))
                             | p <- ps ]
  where ps = vertices g
        n  = length ps

example1 :: [(String, String, Integer)]
example1 = [("a","b",1), ("a","c",1), ("a","d",1),
            ("b","a",1), ("c","a",1), ("d","a",1), ("c","d",1)]

initPageRank' :: Map pageId a -> PageRanks pageId
initPageRank' m = M.map (\x -> PageRank (1 / fromIntegral n)) m
  where n = M.size m

nextPageRank :: (Ord pageId, Edge e pageId, Graph g e pageId) => 
  g -> PageRanks pageId -> pageId -> PageRank
nextPageRank g pr i = (1 - d) / n + d * sum [ pr M.! j / t j 
                                            | j <- s i ]
 where
  d   = 0.85
  n   = fromIntegral (length (vertices g))
  t j = fromIntegral (length (edgesFrom g j))
  s i = map source (edgesTo g i)

nextPageRanks :: Ord pageId => Graph g e pageId =>
  g -> PageRanks pageId -> PageRanks pageId
nextPageRanks g pr = M.mapWithKey (const . nextPageRank g pr) pr

pageRanks :: (Ord pageId, Graph g e pageId) => g -> [PageRanks pageId]
pageRanks g = iterate (nextPageRanks g) (initPageRanks g)

pageRank :: (Ord pageId, Graph g e pageId) =>
  g -> PageRanks pageId
pageRank g = pageRanks g !! 200

nextPageRank' :: (Ord pageId, Edge e pageId, Graph g e pageId) => 
  g -> PageRanks pageId -> PageRank -> pageId -> PageRank -> Maybe PageRank
nextPageRank' g pr k i pri
  | abs (pri - pri') < k  = Nothing
  | otherwise             = Just pri'
 where
   pri' = nextPageRank g pr i

nextPageRanks' :: Ord pageId => Graph g e pageId =>
  g -> PageRank -> PageRanks pageId -> Maybe (PageRanks pageId)
nextPageRanks' g k pr = case M.mapAccumWithKey nextPageRank'' True pr of
                           (True,  pr)  -> Nothing
                           (False, pr') -> Just pr'
  where
    nextPageRank'' converged i pri = case nextPageRank' g pr k i pri of
                            Nothing   -> (converged, pri)
                            Just pri' -> (False, pri')

pageRanks' :: (Ord pageId, Graph g e pageId)
  => g -> PageRank -> [PageRanks pageId]
pageRanks' g k = iterateMaybe (nextPageRanks' g k) (initPageRanks g)
 
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : maybe [] (iterateMaybe f) (f x)

pageRank' :: (Ord pageId, Graph g e pageId) =>
  g -> PageRanks pageId
pageRank' g
  = last . (take 200) $ (pageRanks' g 0.0001 )

example2 :: GameState
example2 = GameState planets wormholes fleets where
  planets = M.fromList
    [ (PlanetId 0, Planet (Owned Player1) (Ships 300) (Growth 7))
    , (PlanetId 1, Planet Neutral         (Ships 200) (Growth 2))
    , (PlanetId 2, Planet Neutral         (Ships 150) (Growth 3))
    , (PlanetId 3, Planet Neutral         (Ships 30)  (Growth 6))
    ]
  wormholes = M.fromList
    [ (WormholeId 0, Wormhole (Source 0) (Target 1) (Turns 1))
    , (WormholeId 1, Wormhole (Source 0) (Target 2) (Turns 1))
    , (WormholeId 2, Wormhole (Source 0) (Target 3) (Turns 1))
    , (WormholeId 3, Wormhole (Source 1) (Target 0) (Turns 1))
    , (WormholeId 4, Wormhole (Source 2) (Target 0) (Turns 1))
    , (WormholeId 5, Wormhole (Source 3) (Target 0) (Turns 1))
    , (WormholeId 6, Wormhole (Source 2) (Target 3) (Turns 1))
    ]
  fleets = []

newtype PlanetRank = PlanetRank Double
  deriving (Num, Eq, Ord, Fractional)
 
type PlanetRanks = Map PlanetId PlanetRank
 
instance Show PlanetRank where
  show (PlanetRank p) = printf "%.4f" p

initPlanetRanks :: GameState -> PlanetRanks
initPlanetRanks g = M.fromList [ (p, PlanetRank (1 / fromIntegral n))
                               | p <- ps ]
  where ps = vertices g
        n  = length ps

planetRank :: GameState -> PlanetRanks
planetRank g = planetRanks g !! 200
 
planetRanks :: GameState -> [PlanetRanks]
planetRanks g = iterate (nextPlanetRanks g) (initPlanetRanks g)

nextPlanetRanks :: GameState -> PlanetRanks -> PlanetRanks
nextPlanetRanks g pr = M.mapWithKey (const . nextPlanetRank g pr) pr

nextPlanetRank :: GameState -> PlanetRanks 
               -> PlanetId -> PlanetRank
nextPlanetRank g@(GameState planets _ _) pr i = 
 (1 - d) / n + d * sum [ pr M.! j * growth i / growths j 
                       | j <- targets i ]
 where
  d   = 0.85
  n   = fromIntegral (length planets)

  growth :: PlanetId -> PlanetRank
  growth i  = (\(Planet _ _ g) -> fromIntegral g) 
                                  (planets M.! i)
  targets :: PlanetId -> [PlanetId]
  targets i 
    = map (\(_, wh) -> target wh) (edgesFrom g i)
 
  growths :: PlanetId -> PlanetRank
  growths j
    = PlanetRank (fromIntegral . sum $ growthNums)
    where 
      ws = wormholesTo (Target j) g
      growthNums = M.map ((\(Planet _ _ g) -> g) . (\x -> lookupPlanet x g) . source) ws 


checkPlanetRanks :: PlanetRanks -> PlanetRank
checkPlanetRanks = sum . M.elems

planetRankRush :: GameState -> AIState 
               -> ([Order], Log, AIState)
planetRankRush gs ai
  | isFirstTime = (orders, [], ai { ranks = planetRank gs })
  | isNothing target = ([], ["There is no more planet to conquer!"], ai)
  | otherwise = (orders, [], ai)
    where isFirstTime = ranks ai == M.empty
          orders = attackFromAll (fromJust target) gs
          target = if isFirstTime then getTarget gs (planetRank gs) else getTarget gs (ranks ai)

getTarget :: GameState -> PlanetRanks -> Maybe PlanetId
getTarget gs rks
  | M.null rks = Nothing
  | owner /= (Owned Player1) = Just pid
  | otherwise = getTarget gs rks'
    where (Planet owner _ _) = lookupPlanet pid gs
          ((pid, _), rks') = deleteAndFindMax rks

deleteAndFindMax :: PlanetRanks -> ((PlanetId, PlanetRank), PlanetRanks)
deleteAndFindMax rks = deleteAndFindMax' allPlanetId (-1, 0)
  where allPlanetId = M.keys rks
        deleteAndFindMax' :: [PlanetId] -> (PlanetId, PlanetRank) -> ((PlanetId, PlanetRank), PlanetRanks)
        deleteAndFindMax' [] p = (p, M.delete (fst p) rks)
        deleteAndFindMax' (x:xs) p
          | r' <= r = deleteAndFindMax' xs p
          | otherwise = deleteAndFindMax' xs (x, r')
            where r' = rks M.! x
                  r  = snd p
{-
Things to consider:
 - turns: Number of turns to take to get to the neighbor planets
 - danger: The danger of being attacked by opponent: so that we need to leave as soon as possible
 - cost: The number of ships need to be compromised to conquer the planet: so that we do not choose that planet if too many sacrifices
 - pagerank: The pagerank of that planet
 - fleet: The opponent's fleet position related to this planet: if there is one opponent fleet that is currently flying to it, then try to avoid the clash
 - knapsack: Using knapsack algorithm before any clashes happen
How to devide ships:
 - if danger is high, then move all the ships to a safer place, or if it is possible, try to conquer one of their planets
-}

canReachOpponentPlanet :: PlanetId -> Planet -> GameState -> Bool
canReachOpponentPlanet pId p g =
  canReachOpponentPlanetHelper pId p g S.empty
  where
    canReachOpponentPlanetHelper :: PlanetId -> Planet -> GameState -> Set PlanetId -> Bool
    canReachOpponentPlanetHelper pId p@(Planet owner _ _) g s
      | owner /= Owned Player1 = True
      | S.member pId s = False
      | otherwise = or (map (\x -> canReachOpponentPlanetHelper x (lookupPlanet x g) g (S.insert x s)) reachablePlanets)
        where 
          edges = edgesFrom g pId
          reachablePlanets = map (\(_, Wormhole _ (Target tId) _) -> tId) edges
  

skynet :: GameState -> AIState
       -> ([Order], Log, AIState)
skynet g@(GameState ps ws fs) ai 
  | M.null (ranks ai) = skynet g ai {ranks = planetRank g}
  | otherwise         = ((M.foldrWithKey generateAttack [] ourPs), [], ai)
  where 
    ourPs = ourPlanets g 

    generateAttack :: PlanetId -> Planet -> [Order] -> [Order] 
    generateAttack pId (Planet _ (Ships s) _) os 
       = concatMap (\(wId, nShips) -> send wId (Just nShips) g) departures ++ os
      where 
        departures =
          if totalShips < 300 then
            let gameStateToOptimize = GameState (M.map (\p@(Planet owner ships growth) -> if owner == Owned Player1 then p else Planet owner (ships + 1) growth) ps) ws fs in
            let bkTargetPlanetIds = snd (optimise gameStateToOptimize (Source pId)) in
            let shipsToSend = map (\pId -> let (Planet _ nShips _) = lookupPlanet pId gameStateToOptimize in nShips ) bkTargetPlanetIds in
            let wormholeIds = concatMap (\tId -> M.keys (M.filter (\(Wormhole s t _) -> (s == Source pId) && (t == Target tId)) ws)) bkTargetPlanetIds in
              zip wormholeIds shipsToSend
          else
            let nPlanetsToConquer = max 1 (div totalShips 150) in
            let wormholeIds = map fst (take nPlanetsToConquer (sortBy (flip cmp) (filter (\(_, Wormhole _ (Target tId) _) -> canReachOpponentPlanet tId (lookupPlanet tId g) g) (edgesFrom g pId)))) in
            let size = length wormholeIds in
            let totalWeight = sum (map (\x -> 1 / (fromIntegral x :: Float)) (take (size + 1) [1..])) in
            let shipsToSend = take size (map (\x -> (Ships (floor ((fromIntegral s :: Float) / totalWeight / (fromIntegral x :: Float))))) [1..])  in -- TODO 
              zip wormholeIds shipsToSend

        totalShips = sum (M.map (\(Planet _ (Ships x) _) -> x) ourPs)
        totalPlanets = M.size ourPs


        cmp :: (WormholeId, Wormhole) -> (WormholeId, Wormhole) -> Ordering
        cmp (_, w1@(Wormhole _ _ (Turns ts1))) (_, w2@(Wormhole _ _ (Turns ts2))) 
          | ourPlanet p1 = LT
          | ourPlanet p2 = GT 
          | otherwise    = compare (t1 / fromIntegral (s1 * ts1 * dr1)) (t2 / fromIntegral (s2 * ts2 * dr2))
          where 
            pId1 = target w1
            pId2 = target w2

            p1@(Planet _ (Ships s1) _) = (lookupPlanet pId1 g)
            p2@(Planet _ (Ships s2) _) = (lookupPlanet pId2 g)

            -- PlanetRank values of two planets
            t1 = (\(PlanetRank d) -> d) ((ranks ai) M.! pId1)
            t2 = (\(PlanetRank d) -> d) ((ranks ai) M.! pId2)

            -- danger ratings of two planets
            dr1 = dangerRating pId1 g
            dr2 = dangerRating pId2 g

turnsToTake :: PlanetId -> PlanetId -> Turns
turnsToTake srcpid destpid = undefined

dangerPoint :: GameState -> Int
dangerPoint gs = undefined

-- coumpute sum of opponent ships in adjacent vertexs 
-- and multiply of wieght for all adjacent enemy vertices to attect the vertice
-- and all incomming enemy fleets amount times the remaning turns
dangerRating :: PlanetId -> GameState -> Int
dangerRating pId g 
  = foldl drHelp 0 (edgesFrom g pId) 
  where
    drHelp :: Int -> (WormholeId, Wormhole) -> Int
    drHelp n (wId, w) 
      | enemyPlanet p = n + s * (fromInteger (weight w))
      | otherwise     = n
      where 
        p@(Planet _ (Ships s) _) = lookupPlanet (target w) g 

cost :: PlanetId -> Ships
cost targetpid = undefined

pagerank :: GameState -> AIState -> PlanetRank
pagerank gs ai = undefined

fleet :: GameState -> PlanetId -> Int
fleet gs pid = undefined

choose :: GameState -> PlanetId -> (Growth, [PlanetId])
choose gs srcpid = undefined

deriving instance Generic PlanetRank
deriving instance Generic PageRank
 
instance NFData PageRank
instance NFData PlanetRank
