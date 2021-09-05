
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
import Data.List (concatMap, sortBy, nub)
import Data.Maybe ( isNothing, fromJust )
import Data.Set (Set)
import qualified Data.Set as S
import Text.Printf
import Control.DeepSeq
import GHC.Generics

import System.Random (StdGen, mkStdGen, randomR)


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
  , adjMap :: Map PlanetId [PlanetId]
  , planetWormholeMap :: Map (PlanetId, PlanetId) WormholeId
  , seed :: StdGen 
  } deriving Generic
 
initialState :: AIState
initialState = AIState
  { turn = 0
  , rushTarget = Nothing
  , ranks = M.empty 
  , adjMap = M.empty
  , planetWormholeMap = M.empty
  , seed = mkStdGen 42
  }
  
type Log = [String]

-- get enemy fleets that are heading towards friend planets
logIncomingFleet :: GameState -> String
logIncomingFleet g@(GameState ps ws fs)
  = show (filter (\(pId, player, _, _) -> (elem pId (M.keys friendPs)) && (player == Player2)) [ (target (lookupWormhole wId g), player, s, t) | (Fleet player s wId t) <- fs ])
  where 
    friendPs = ourPlanets g

-- get friend fleets that are heading towards other planets
logAttackingFleet :: GameState -> String
logAttackingFleet g@(GameState ps ws fs)
  = show (filter (\(pId, player, _, _) -> (elem pId (M.keys enemyPs)) && (player == Player1)) [ (target (lookupWormhole wId g), player, s, t) | (Fleet player s wId t) <- fs ])
  where 
    enemyPs = M.filter (not . ourPlanet) ps

-- get friend fleet transfering between friend planets
logTransferFleet :: GameState -> String 
logTransferFleet g@(GameState ps ws fs)
  = show (filter (\(pId, player, _, _) -> (elem pId (M.keys friendPs)) && (player == Player1)) [ (target (lookupWormhole wId g), player, s, t) | (Fleet player s wId t) <- fs ])
  where 
    friendPs = ourPlanets g

-- get friend planet ids
logFriendPlanet :: GameState -> String
logFriendPlanet g@(GameState ps ws fs)
  = show (ourPlanets g)

-- ai that print the necessary information for debug
pacifist :: GameState -> AIState -> ([Order], Log, AIState)
pacifist g@(GameState ps ws fs) ai 
  = ([], 
     ["friend: " ++ (logFriendPlanet g), 
      "incoming fleets: " ++ (logIncomingFleet g), 
      "attacking fleets: " ++ (logAttackingFleet g)],
     ai)

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
zergRush g ai
  | t == Nothing || o == Player1 = case nextEnemy of  
          Nothing -> ([], [], ai) 
          _       -> zergRush g ai {rushTarget = nextEnemy }
  | otherwise = (attackFromAll tId g, 
                  ["friend: " ++ (logFriendPlanet g), 
                  "incoming fleets: " ++ (logIncomingFleet g), 
                  "attacking fleets: " ++ (logAttackingFleet g), 
                  "transfer fleet: " ++ (logTransferFleet g)],
                  ai)
  where 
    nextEnemy = findEnemyPlanet g
    t = rushTarget ai
    tId = fromJust t
    target@(Planet (Owned o) _ _) = lookupPlanet tId g
    
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


{- previous code which does not use reinforce learning
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
            let shipsToSend = take size (map (\x -> (Ships (floor ((fromIntegral s :: Float) / totalWeight / (fromIntegral x :: Float))))) [1..]) in
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
-}

type PlanetFeature = Map PlanetId [Double] -- neural net feature of a planet

feature_num = 6
-- index of features 
gen_index = 0 -- index of generate rate feature
amount_index = 1
is_friend = 2
surround_index = 3
friend_to_index = 4
enemy_to_index = 5

-- index of outputs
valueIndex = 0
attRatioIndex = 1

-- parameters used to explore parameters
epsilon = 0.1 -- epsilon = 0.2

-- network parameters
-- network contains 2 hidden layers, with 8 neurons, and one output layer, with 2 neurons 
hidden1_w = [[-0.4182015359401703, -0.19266201555728912, 0.16139177978038788, -0.37729737162590027, -0.3881824016571045, -0.36062556505203247], [-0.13083845376968384, 0.7202723622322083, -0.35876473784446716, 0.43172264099121094, -0.014516857452690601, 0.3789520859718323], [-0.15188346803188324, -0.4650476276874542, -0.26168280839920044, 0.2193392664194107, 0.28123408555984497, 0.029286645352840424], [-0.17302396893501282, 0.46063220500946045, -0.19785478711128235, -0.33777087926864624, 0.35103750228881836, 0.25987955927848816], [-0.06531905382871628, -0.10485851764678955, 0.050701942294836044, 0.2868793308734894, 0.067925363779068, -0.5832029581069946], [-0.15944337844848633, -0.34577035903930664, -0.2049867808818817, 0.3570820391178131, 0.3968364894390106, -0.0656164288520813], [-0.331567645072937, -0.1998482048511505, 0.1883271485567093, 0.13623948395252228, 0.10405832529067993, -0.49895840883255005], [-0.4305676519870758, 0.1370033323764801, 0.07165958732366562, 0.04977180063724518, -0.009986676275730133, -0.4323834478855133]]
hidden1_b = [-0.08822999894618988, 0.23545946180820465, -0.33256056904792786, 0.27731531858444214, 0.0958804115653038, -0.13986153900623322, 0.24286359548568726, 0.056623995304107666]
hidden2_w = [[0.08571185171604156, 0.939017117023468, -0.2636966407299042, 0.5419144034385681, 0.0612005740404129, 0.04886515066027641, 0.13445936143398285, 0.03512562811374664], [0.3324899673461914, 0.28762686252593994, 0.306379109621048, -0.13149556517601013, 0.3687320053577423, 0.03978849574923515, 0.4085516035556793, -0.42942875623703003], [-0.28293538093566895, -0.04527994990348816, 0.16790470480918884, -0.3481031656265259, -0.0830756425857544, -0.062048882246017456, 0.08843570947647095, 0.2851979434490204], [-0.34683942794799805, -0.30261069536209106, 0.030041009187698364, 0.19150778651237488, -0.17409838736057281, 0.028928279876708984, -0.044718652963638306, 0.054142892360687256], [-0.10815663635730743, -0.09868276119232178, 0.030322568491101265, -0.43911299109458923, 0.08857674896717072, 0.018361896276474, -0.24347688257694244, -0.3158162534236908], [-0.36184704303741455, -0.21142493188381195, 0.018703309819102287, -0.5159909725189209, -0.14280612766742706, 0.26584914326667786, 0.03227526694536209, 0.13898737728595734], [0.17940931022167206, -0.11088082194328308, -0.20199505984783173, 0.1342603713274002, 0.11329477280378342, -0.18528322875499725, 0.2554776966571808, -0.4303998649120331], [0.4242788255214691, -0.6295784115791321, -0.15585514903068542, -0.46718350052833557, 0.01592428609728813, -0.33571380376815796, 0.17796169221401215, 0.3155672252178192]]
hidden2_b = [-0.06079117953777313, 0.35528644919395447, 0.1648866832256317, 0.07368001341819763, -0.0660535916686058, -0.27329692244529724, 0.22593680024147034, 0.0691629946231842]
output_w = [[-0.4530307948589325, -0.24567264318466187, -0.337292343378067, 0.2365145981311798, 0.3075406551361084, -0.004160591401159763, -0.3799457848072052, -0.17263059318065643], [1.017461895942688, 0.3578270971775055, -0.08351266384124756, -0.20811042189598083, 0.06662522256374359, 0.17096659541130066, -0.24399533867835999, -0.22668153047561646]]
output_b = [-0.41888874769210815, -0.4851835072040558]

-- get map from each planet to its adjacent planet
getAdjMap :: GameState -> Map PlanetId [PlanetId]
getAdjMap g@(GameState ps ws _)
  = M.foldr (\(Wormhole (Source s) (Target t) _) m -> M.adjust (\list -> t : list) s m) emptyMap ws
  where 
    -- create empty map contain all planet ids but each has empty adj list
    emptyMap = M.foldrWithKey (\pId _ m -> M.insert pId [] m ) M.empty ps

-- get list of planets adjacent to given planets
getAdjacentPlanets :: [PlanetId] -> AIState -> [PlanetId]
getAdjacentPlanets startPIds ai
  = nub . concat . M.elems . M.filterWithKey (\k _ -> elem k startPIds) $ (adjMap ai)

-- get map from planet Id pair to wormhole Id
getPlanetWormMap :: GameState -> Map (PlanetId, PlanetId) WormholeId
getPlanetWormMap g@(GameState _ ws _)
  = M.foldrWithKey (\wId w m -> M.insert (source w, target w) wId m) M.empty ws 


-- get Features for each planet, parameters will be passed to neural net for calculation
generateFeatures :: GameState -> [PlanetId] -> PlanetFeature
generateFeatures gs@(GameState allPs ws fs) adjPs 
  = featureTable''
  where 
    PlanetId maxPlanetId = maximum adjPs
    enemyPlanets = M.filter enemyPlanet allPs

    featureTable :: PlanetFeature

    -- 1. fill in feature value of gen rate, ship number, is friend; leave surrounding, frind, enemy fleet features zero
    featureTable = foldr (\pId m -> M.insert pId (getGenVal pId ++ (replicate (feature_num - 3) 0)) m) M.empty adjPs
    -- 2. get total surrounding enemy number
    featureTable' = M.foldrWithKey (\ek ep m -> updateSurroundFeature m ek ep) featureTable enemyPlanets
    -- 3. get fleet transfering to the planet
    featureTable'' = foldr (\(Fleet owner s w t) m -> updateFleetFeature m owner s w t) featureTable' fs
    
    -- get generate rate, ship number, is friend of planet with given pId
    getGenVal :: PlanetId -> [Double]
    getGenVal pId 
      = [fromIntegral g, fromIntegral s, isFriend]
      where 
        (Planet owner (Ships s) (Growth g)) = lookupPlanet pId gs
        isFriend = if owner == Owned Player1 then 1 
                   else if owner == Owned Player2 then -1
                   else 0
                    
    -- update surrounding enemy ship feature
    updateSurroundFeature :: PlanetFeature -> PlanetId -> Planet -> PlanetFeature
    updateSurroundFeature m pId (Planet _ (Ships s) _)
      = foldr (\(Wormhole _ (Target target_pId) _) m' -> M.adjust (\list -> addInPlace list surround_index (fromIntegral s)) target_pId m') m ws 
      where 
        ws = M.elems (wormholesFrom (Source pId) gs)


    -- update features in table about fleet moving, Return Updated Map 
    updateFleetFeature :: PlanetFeature -> Player -> Ships -> WormholeId -> Turns -> PlanetFeature
    updateFleetFeature prevMap owner (Ships s) wId (Turns t)
      = M.adjust addedFeature targetPId prevMap
      where 
        targetPId = target (lookupWormhole wId gs)
        friendFleet = if owner == Player1 then (fromIntegral s) / (fromIntegral t) else 0
        enemyFleet  = if owner == Player2 then (fromIntegral s) / (fromIntegral t) else 0

        addedFeature = (\list -> addInPlace 
                                   (addInPlace list enemy_to_index enemyFleet)
                                   friend_to_index 
                                   friendFleet)

    -- return a list, which add a given value on given position
    addInPlace :: [Double] -> Int -> Double -> [Double]
    addInPlace list index val 
      = [ if i == index 
          then val + list !! i
          else list !! i
          | i <- [0..feature_num-1]]

-- forward a batch of sample through neural net, generate 2 values for each planet
-- batch size is varying, equal to map size provided
forward_batch :: PlanetFeature -> PlanetFeature 
forward_batch m 
  = M.map (forward) m
  where 
    -- relu activation function
    relu :: [Double] -> [Double]
    relu 
      = map (max 0)

    -- sigmoid activation function, used to cap attackRatio of each planet to [0, 1] range
    sigmoid :: [Double] -> [Double]
    sigmoid 
      = map (\x -> 1 / (1 + exp (-x)))

    -- forward one sample through one neural net layer
    forward_one :: [Double] -> [[Double]] -> [Double] -> [Double]
    forward_one x weight_matrix bias_vector
      = map (\(b, x) -> b + x) (zip bias_vector [sum (map (\(w, x) -> w * x) (zip w x)) | w <- weight_matrix])

    -- forward one sample through the neural net
    forward :: [Double] -> [Double]
    forward x
      = output
      where 
        layer1_out = relu (forward_one x hidden1_w hidden1_b)
        layer2_out = relu (forward_one layer1_out hidden2_w hidden1_b)
        output = sigmoid (forward_one layer2_out output_w output_b)

-- for each planet in the given map explore random value around its forward result 
-- function ensured no two explored value use the same seed for rand generation
-- used to explore better parameter and calculate gradient in reinforce learning
explore :: PlanetFeature -> AIState -> (AIState, PlanetFeature)
explore m ai 
  = M.foldrWithKey (\pId outList (ai', m') -> explore' ai' m' pId outList) (ai, M.empty) m
  where 
    -- for one pId-output map entity, generate its explored output and add to result feature map
    explore' :: AIState -> PlanetFeature -> PlanetId -> [Double] -> (AIState, PlanetFeature)
    explore' ai m pId outList
      = (ai', M.insert pId exploredNetList m)
      where 
        (ai', exploredNetList) = foldr oneRand (ai, []) outList

    -- append one random value to the pair
    oneRand :: Double -> (AIState, [Double]) -> (AIState, [Double])
    oneRand v (ai, ls)
      = (ai {seed = seed'} , clamp (v + rand) : ls)
      where
        (rand, seed') = randomR (-epsilon, epsilon) (seed ai)
    
    -- clamp value to range [0, 1]
    clamp :: Double -> Double
    clamp 
      = max 0 . (min 1)

-- based on neural net generated results, randomly choose value around the range as value used in generating attack, 
-- then generate attack orders
generateAttack :: [PlanetId] -> PlanetFeature -> GameState -> AIState -> [Order]
generateAttack friends m g ai
  = orders
  where 
    planetWormMap = (planetWormholeMap ai) 

    orders = 
      concat [ -- concat different planets' orders
      map -- for each planet, generate its attack orders
      (\(targetPId, v) -> 
        let 
        wId = planetWormMap M.! (pId, targetPId); -- the wormhole planet with pId is sending attach through
        attackRatio = (m M.! pId) !! attRatioIndex; -- the percentage of ship sent to attack
        (Planet _ (Ships totShipNum) _) = lookupPlanet pId g; -- total amount of ship in planet pId
        ships = Ships . floor $ (v * attackRatio * (fromIntegral totShipNum))  -- the attack ship amount
        in (Order wId ships)
      )
      adjPs
      | (pId, adjPs) <- M.toList record']

    -- record planet value around each friend planet
    record :: Map PlanetId [(PlanetId, Double)]
    record = (M.map (\vs -> map (\p -> (p, (m M.! p) !! valueIndex)) vs)) . (M.filterWithKey (\k _ -> elem k friends)) $ (adjMap ai)

    -- record that change planet value to softmax value
    record' = M.map softmax record

    -- softmax function applied to one planet's adjacent planets' values
    softmax :: [(PlanetId, Double)] -> [(PlanetId, Double)]
    softmax list 
      = map (\(pId, v) -> (pId, v / expValSum)) expList 
      where 
        expList = map (\(pId, v) -> (pId, exp v)) list
        expValSum = sum (map snd expList)

-- get reward for last step of training
-- reward is calculated as (the sum of growth)
getReward :: [Planet] -> Double
getReward ps
  = fromIntegral . sum $ [ g | (Planet _ _ (Growth g)) <- ps]

-- -- reward is calculated as (difference in the sum of growth + 0.1 * difference in ship sum)
-- getReward :: GameState -> Double
-- getReward g@(GameState ps _ fs)
--   = 
--   where 
--     (growthDiff, shipDiff) = foldr (\() -> ) (0, 0) ps 

-- the GREATEST ai that use reinforce learning
skynet :: GameState -> AIState
       -> ([Order], Log, AIState)
skynet g@(GameState ps ws fs) ai 
  | adjMap ai == M.empty = skynet g (ai {adjMap = getAdjMap g, planetWormholeMap = getPlanetWormMap g})
  | length friendPlanetIds == 0 = ([], [], ai) -- if no friend planet, do no action
  | otherwise 
    = (orders, 
        -- output in format: 
        --   {'reward' : last step's reward, double value
        --     'feature': all adjacent planet's feature list, 2d array
        --     'vals': all adjacent planet's explored vals, 2d array } 
        ["{ 'reward': " ++ (show reward) ++
        ", 'vals' : " ++ ((show . M.elems) $ exploredNetOutputs) ++ 
        ", 'features': " ++ ((show . M.elems) $ features) ++ 
        "}"],
      ai') 
  where 
    -- get all examined planets, including friend planets and adjacent planets
    friendPlanets = ourPlanets g
    friendPlanetIds = M.keys friendPlanets
    adjacent_planets = getAdjacentPlanets friendPlanetIds ai

    -- get feature of each planet, forward through network
    features = generateFeatures g (nub (concat [adjacent_planets, friendPlanetIds]))
    netOutputs = forward_batch features

    -- explore values by taking values in (-eplison, epsilon) range
    (ai', exploredNetOutputs) = explore netOutputs ai

    -- generate attack orders
    orders = generateAttack friendPlanetIds exploredNetOutputs g ai'

    -- get reward of last training step, value is copied to the start of every list in this step's log
    -- reward = getReward g
    reward = getReward (M.elems friendPlanets)

deriving instance Generic PlanetRank
deriving instance Generic PageRank
 
instance NFData PageRank
instance NFData PlanetRank
