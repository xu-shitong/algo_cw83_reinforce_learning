
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
  , defensiveLevel :: Double
  } deriving Generic
 
initialState :: AIState
initialState = AIState
  { turn = 0
  , rushTarget = Nothing
  , ranks = M.empty 
  , strategyPoints = M.empty
  , params = [0, 0, 0, 0]
  , defensiveLevel = 0
  }

type Log = [String]

pacifist :: GameState -> AIState -> ([Order], Log, AIState)
pacifist _ ai = ([], ["Do no harm."], ai)

enemyPlanet :: Planet -> Bool
enemyPlanet (Planet (Owned Player2) _ _) = True
enemyPlanet _                            = False

findEnemyPlanet :: GameState -> Maybe PlanetId
findEnemyPlanet g@(GameState ps _ _)
  | M.null enemies = Nothing
  | otherwise      = Just . fst . head . M.toList $ enemies
  where 
    enemies = M.filter enemyPlanet ps

send :: WormholeId -> Maybe Ships -> GameState -> [Order]
send wId ms st 
  | o == Neutral || o /= (Owned Player1) = []
  | isNothing ms || amount >= totalShips = [(Order wId (Ships totalShips))]
  | otherwise                            = [(Order wId mShips)]
  where
    (Just mShips@(Ships amount)) = ms
    Wormhole (Source src) _ _ = lookupWormhole wId st
    planet@(Planet o (Ships totalShips) _) = lookupPlanet src st

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
attackFromAll targetId gs@(GameState ps ws fs)
  = concat [ send wId Nothing gs | (wId, w@(Wormhole _ (Target t) _)) <- nextWormholes]
  where 

    ourPs = M.toList (ourPlanets gs)
    maybePaths = [ shortestPath pId targetId gs | (pId, p@(Planet _ s _)) <- ourPs]
    nextWormholes = map (\(Just (Path _ es)) -> last es) (filter (\x -> not (isNothing x)) maybePaths)

zergRush :: GameState -> AIState 
         -> ([Order], Log, AIState)
zergRush gs ai
  | t == Nothing = zergRush gs ai {rushTarget = findEnemyPlanet gs}
  | o == Player1 = zergRush gs ai {rushTarget = findEnemyPlanet gs}
  | otherwise    = (attackFromAll tId gs, ["attacking " ++ (show tId)], ai)
  where 
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
initPageRank' m 
  = M.map (\x -> (PageRank (1 / fromIntegral n))) m
  where 
    n = M.size m

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
planetRankRush g ai
  | turn ai == 0 = planetRankRush g ai {ranks = calRank}
  | t == Nothing = case (M.null enemyRanks) of
                   True  -> ([], ["no ememy or nutral planet found"], ai)
                   False -> planetRankRush g ai {rushTarget = (Just pId)}
  | otherwise    = (attackFromAll tId g, ["attacking " ++ (show tId)], ai)
  where 
    calRank = planetRank g 
    enemyRanks = M.filterWithKey (\pId r -> not (ourPlanet (lookupPlanet pId g))) (ranks ai)
    (pId, r) = M.findMax enemyRanks
    t = rushTarget ai
    tId = fromJust t

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
  | turn ai == 0 = skynet g ai {ranks = calRank}
  | otherwise = (M.foldrWithKey generateAttack [] ourPs, [], ai)
  where 
    calRank = planetRank g 
    ourPs = ourPlanets g 

    generateAttack :: PlanetId -> Planet -> [Order] -> [Order] 
    generateAttack pId (Planet _ (Ships s) _) os 
      = [ (Order wId (Ships (round (totAttack / rank)))) | (pId', wId, rank) <- targets ]
      where 
        totRanks = sum . (map (\(_, _, x) -> x)) $ targets 
        targets = applyParams pId g ai 
        currDefenceRate = dangerRaiting pId g

        -- +1 here is to prevent divide by 0 error
        totAttack = (fromIntegral s) * ((defensiveLevel ai) * totRanks) / (fromIntegral (currDefenceRate + 1) + totRanks)

-- given a planet id, return orders to send ships to neighbours
applyParams :: PlanetId -> GameState -> AIState -> [(PlanetId, WormholeId, Double)]
applyParams pId g ai 
  = [ (pId,
      wId, 
      pGro * (fromIntegral gro) + 
      pPr * pr + 
      pT * t -
      pDr * fromIntegral dr) | (pId, wId, gro, (PlanetRank pr), t, dr) <- adjList]
  where 
    (pGro : pPr : pT : pDr : []) = params ai 
    currP = lookupPlanet pId g 
    adjList = [ ( target w
                , wId
                , (\(Planet _ _ (Growth g)) -> g) (lookupPlanet (target w) g)
                , (ranks ai) M.! (target w) 
                , fromInteger (weight w) 
                , dangerRaiting (target w)  g)
                | (wId, w) <- edgesFrom g pId ]

-- coumpute sum of opponent ships in adjacent vertexs 
-- and multiply of wieght for all adjacent enemy vertices to attect the vertice
dangerRaiting :: PlanetId -> GameState -> Int
dangerRaiting pId g 
  = foldl drHelp 0 (edgesFrom g pId)
  where 

    drHelp :: Int -> (WormholeId, Wormhole) -> Int
    drHelp n (_, w) 
      | enemyPlanet p = n + s * (fromInteger (weight w))
      | otherwise     = n
      where 
        p@(Planet _ (Ships s) _) = lookupPlanet (target w) g 

deriving instance Generic PlanetRank
deriving instance Generic PageRank
 
instance NFData PageRank
instance NFData PlanetRank
