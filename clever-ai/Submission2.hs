
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
  | t == Nothing || o == Player1 = zergRush gs ai {rushTarget = findEnemyPlanet gs}
  | otherwise = (attackFromAll tId gs, ["attacking " ++ (show tId)], ai)
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
  | isFirstTime = (orders, [], ai { planetRanking = Just rank })
  | isNothing target = ([], ["There is no more planet to conquer!"], ai)
  | otherwise = (orders, [], ai)
    where isFirstTime = planetRanking ai == Nothing
          rank = planetRank gs
          orders = attackFromAll (fromJust target) gs
          target = if isFirstTime then getTarget gs rank else getTarget gs (fromJust (planetRanking ai))

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

type Gain = Double

computeGain :: GameState -> AIState -> PlanetId -> PlanetId -> [Double] -> (Gain, Log, AIState)
computeGain gs ai srcpid destpid params
  | isFirstTime = (gain, ["Gain computed"], ai { planetRanking = Just rank } )
  | otherwise   = (gain, ["Gain computed"], ai )
      where isFirstTime = planetRanking ai == Nothing
            gain = (params !! 0) * pagerank + (params !! 1) * (fromInteger turns) + (params !! 2) * (fromIntegral damaged) + (params !! 3) * (fromIntegral compromised)
            pagerank :: Double
            pagerank
              | planetRanking ai == Nothing = let (PlanetRank res) = rank M.! destpid in res
              | otherwise = let (PlanetRank res) = (fromJust (planetRanking ai)) M.! destpid in res
            (Path turns _) = (fromJust (shortestPath srcpid destpid gs))
            damaged :: Int
            damaged
              | destOwner == (Owned Player2) && destShips < srcShips = destShips             --reward here
              | destOwner == (Owned Player2) && destShips >= srcShips = srcShips - destShips --punishment here
              | otherwise = 0
            compromised :: Int
            compromised
              | destOwner /= (Owned Player1) && destShips < srcShips = destShips --punishment here
              | destOwner /= (Owned Player1) && destShips >= srcShips = srcShips - destShips --punishment here
              | otherwise = 0
            rank   = planetRank gs
            target = lookupPlanet destpid gs
            (Planet destOwner (Ships destShips) destGrowth) = lookupPlanet destpid gs
            (Planet _ (Ships srcShips) srcGrowth) = lookupPlanet srcpid gs

computeDecision :: GameState -> AIState -> [Double] -> ([Order], Log, AIState)
computeDecision gs ai params = (order, [], ai { gain = (gain ai) + g })
  where order = undefined --should return the wormhole corresponding to the attackSrc and attackDest
        ((g, _, _), attackSrc, attackDest) = maximumBy cmp gains
        gains = concat [ [ (computeGain gs ai srcpid destpid params, srcpid, destpid) | destpid <- map target (edgesFrom gs srcpid) ] | srcpid <- ourPlanets ]
        ourPlanets = filter (\x -> ourPlanet (lookupPlanet x gs)) (vertices gs)
        cmp :: Ord a => (a, b, c) -> (a, b, c) -> Ordering
        cmp ((g1, _, _), _, _) ((g2, _, _), _, _)
          | g1 > g2 = GT
          | g1 < g2 = LT
          | otherwise = EQ

deriving instance Generic PlanetRank
deriving instance Generic PageRank
 
instance NFData PageRank
instance NFData PlanetRank
