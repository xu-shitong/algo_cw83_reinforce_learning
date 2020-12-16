
{-#  LANGUAGE GeneralizedNewtypeDeriving  #-}
{-#  LANGUAGE StandaloneDeriving  #-}
{-#  LANGUAGE DeriveGeneric  #-}

module Submission2 where
import Lib

  hiding (example1, example2, example3, lookupPlanet)
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (unfoldr)
import Data.List
import Data.Maybe
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
  | isFirstTime = (orders, [], ai { ranks = ranking })
  | isNothing target = ([], ["There is no more planet to conquer!"], ai)
  | otherwise = (orders, [], ai)
    where isFirstTime = ranks ai == M.empty
          ranking = planetRank gs
          orders = attackFromAll (fromJust target) gs
          target = if isFirstTime then getTarget gs ranking else getTarget gs (ranks ai)

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
  in the first turn, 
  calculate:
    the overall importance of each planet :: planetRanks
    all planet's rank :: planetRanks
  predefine: 
    (todo) parameters for each factor :: [Double]
  store in aistate
-}
skynet :: GameState -> AIState
       -> ([Order], Log, AIState)
skynet g ai 
  | M.null (ranks ai) = skynet g ai {ranks = calRank}
  | otherwise         = ((M.foldrWithKey generateAttack [] ourPs), [], ai)
  where 
    calRank = planetRank g 
    ourPs = ourPlanets g 

    generateAttack :: PlanetId -> Planet -> [Order] -> [Order] 
    generateAttack pId (Planet _ (Ships s) _) os 
      | otherwise = (send wId Nothing g) ++ os
      where 
        (wId, _) = maximumBy cmp (edgesFrom g pId)

        cmp :: (WormholeId, Wormhole) -> (WormholeId, Wormhole) -> Ordering
        cmp (_, w1@(Wormhole _ _ (Turns ts1))) (_, w2@(Wormhole _ _ (Turns ts2))) 
          | ourPlanet p1 = LT
          | ourPlanet p2 = GT 
          | otherwise    = compare (t1 / fromIntegral (s1 * ts1)) (t2 / fromIntegral (s2 * ts2))
          where 
            pId1 = target w1
            pId2 = target w2
            p1@(Planet _ (Ships s1) _) = (lookupPlanet pId1 g)
            p2@(Planet _ (Ships s2) _) = (lookupPlanet pId2 g)

            t1 = (\(PlanetRank d) -> d) ((ranks ai) M.! pId1)
            t2 = (\(PlanetRank d) -> d) ((ranks ai) M.! pId2)

{-
skynet g ai
  | M.null (ranks ai) = skynet g ai {ranks = calRank}
  | otherwise         = ((M.foldrWithKey generateAttack [] ourPs), [(show (sum [ s | (Order _ (Ships s)) <- os ]))], ai)
  where 
    os = (M.foldrWithKey generateAttack [] ourPs)
    calRank = planetRank g 
    ourPs = ourPlanets g 
    outputGameStatus :: String 
    outputGameStatus 
      = "#" ++ show (shipSum + fleetSum)
      where 
        shipSum = sum [ calShips pId | pId <- vertices g]
        fleetSum = sum [ if p == Player1 then s else -s | f@(Fleet p (Ships s) wId (Turns t)) <- fs ]

        (GameState _ _ fs) = g
        calShips :: PlanetId -> Int 
        calShips id 
          | ourPlanet p   = s
          | enemyPlanet p = -s 
          | otherwise     = 0
          where 
            p@(Planet _ (Ships s) _) = lookupPlanet id g

    generateAttack :: PlanetId -> Planet -> [Order] -> [Order] 
    generateAttack pId (Planet _ (Ships s) _) os 
      = [ (Order wId (Ships ((\x -> if x < 0 then 0 else x) . floor $ (totAttack * rank / totRanks)))) | (pId', wId, rank) <- targets ] ++ os
      where 
        totRanks = sum . filter (\x -> x > 0) . (map (\(_, _, x) -> x)) $ targets 
        targets = applyParams pId g ai 
        currDefenceRate = dangerRating pId g

        (_ : _ : _ : _ : offensiveLevel : []) = params ai 
        -- +1 here is to prevent divide by 0 error
        totAttack = ((fromIntegral s) * (offensiveLevel * totRanks)) / (fromIntegral (currDefenceRate + 1) + (offensiveLevel * totRanks))

-- given a planet id, return orders to send ships to neighbours
applyParams :: PlanetId -> GameState -> AIState -> [(PlanetId, WormholeId, Double)]
applyParams pId g ai 
  = [ (pId,
      wId, 
      pGro * (fromIntegral gro) + 
      pPr * pr -
      pT * t -
      pDr * fromIntegral dr) | (pId, wId, gro, (PlanetRank pr), t, dr) <- adjList]
  where 
    (pGro : pPr : pT : pDr : _) = params ai 
    currP = lookupPlanet pId g 
    adjList = [ ( target w
                , wId
                , (\(Planet _ _ (Growth g)) -> g) (lookupPlanet (target w) g)
                , (ranks ai) M.! (target w)
                , fromInteger (weight w) 
                , dangerRating (target w)  g)
                | (wId, w) <- edgesFrom g pId ]
-}

-- coumpute sum of opponent ships in adjacent vertexs 
-- and multiply of wieght for all adjacent enemy vertices to attect the vertice
-- and all incomming enemy fleets amount times the remaning turns
dangerRating :: PlanetId -> GameState -> Int
dangerRating pId g 
  = foldl drHelp 0 (edgesFrom g pId) + sum [ s * t | f@(Fleet p (Ships s) wId (Turns t)) <- fs, p == Player2, pId == target (lookupWormhole wId g) ]
  where 
    (GameState _ _ fs) = g

    drHelp :: Int -> (WormholeId, Wormhole) -> Int
    drHelp n (wId, w) 
      | enemyPlanet p = n + s * (fromInteger (weight w))
      | otherwise     = n
      where 
        p@(Planet _ (Ships s) _) = lookupPlanet (target w) g 

deriving instance Generic PlanetRank
deriving instance Generic PageRank
 
instance NFData PageRank
instance NFData PlanetRank
