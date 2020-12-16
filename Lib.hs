
{-#  OPTIONS_GHC -Wall  #-}
{-#  OPTIONS_GHC -Wno-unused-matches  #-}
{-#  OPTIONS_GHC -Wno-name-shadowing  #-}
{-#  OPTIONS_GHC -Wno-incomplete-patterns  #-}

{-#  LANGUAGE DeriveGeneric  #-}
{-#  LANGUAGE FlexibleInstances  #-}
{-#  LANGUAGE ScopedTypeVariables  #-}
{-#  LANGUAGE FunctionalDependencies  #-}
{-#  LANGUAGE GeneralizedNewtypeDeriving  #-}
{-#  LANGUAGE StandaloneDeriving  #-}
{-#  LANGUAGE InstanceSigs  #-}
{-#  LANGUAGE UndecidableInstances  #-}
{-#  LANGUAGE TypeApplications  #-}

module Lib
  ( Edge (..)
  , Weight
  , GameState (..)
  , Graph (..)
  , Growth (..)
  , Order (..)
  , Owner (..)
  , Path (..)
  , Planet (..)
  , PlanetId (..)
  , Planets
  , Player (..)
  , Ships (..)
  , Source (..)
  , Target (..)
  , Turns (..)
  , Fleet (..)
  , Fleets
  , Wormholes
  , AdjList (..)
  , Heap (..)
  , Tree (..)
  , wormholesFrom
  , wormholesTo
  , Wormhole (..)
  , WormholeId (..)
  , WormholeWithId (..)
  , shortestPaths
  , PQueue (..)
  , lt
  , gt
  , lte
  , eq
  , maxBy
  , tabulate
  , conflictZones
  , optimise
  ) where

import Prelude hiding (maximum)

import Control.DeepSeq
import Data.Coerce (coerce)

import Data.Array
import Data.Maybe
import Data.List (unfoldr, nub, sortBy, (\\))
import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Binary as B
import           GHC.Generics

data Player = Player1 | Player2

data Planet = Planet Owner Ships Growth
newtype Ships = Ships Int
newtype Growth = Growth Int

data Owner = Neutral | Owned Player

newtype PlanetId = PlanetId Int

type Planets = Map PlanetId Planet

data Wormhole = Wormhole Source Target Turns

newtype Source = Source PlanetId
newtype Target = Target PlanetId
newtype Turns  = Turns Int

newtype WormholeId = WormholeId Int

type Wormholes = Map WormholeId Wormhole

data Fleet = Fleet Player Ships WormholeId Turns

type Fleets = [Fleet]

data GameState = GameState Planets Wormholes Fleets

data Order = Order WormholeId Ships

tabulate :: Ix i => (i,i) -> (i -> a) -> Array i a
tabulate (u,v) f = array (u,v) [ (i, f i) | i <- range (u, v)]

wormholesFrom :: Source -> GameState -> Wormholes
wormholesFrom pId (GameState _ ws _)
  = M.filter (\(Wormhole s _ _) -> s == pId) ws

wormholesTo :: Target -> GameState -> Wormholes
wormholesTo pId (GameState _ ws _)
  = M.filter (\(Wormhole _ t _) -> t == pId) ws

maxBy :: Ord b => (a -> b) -> a -> a -> a
maxBy f x y = case compare (f x) (f y) of
    GT -> x
    _  -> y

type Weight = Integer

class Eq v => Edge e v | e -> v where
  source :: e -> v
  target :: e -> v
  weight :: e -> Weight

instance Edge (String, String, Integer) String where
  source (s, _, _) = s
  target (_, t, _) = t
  weight (_, _, i) = i

instance Edge Wormhole PlanetId where
  source (Wormhole (Source s) _ _)    = s
  target (Wormhole _ (Target t) _)    = t
  weight (Wormhole _ _ (Turns turns)) = toInteger turns

instance Edge (WormholeId, Wormhole) PlanetId where
  source (_, w) = source w
  target (_, w) = target w
  weight (_, w) = weight w

data Path e = Path Weight [e]

pathFromEdge :: Edge e v => e -> Path e
pathFromEdge e = Path (weight e) [e]

extend :: Edge e v => Path e -> e -> Path e
extend (Path _ []) _ = error "extend: Empty path"
extend (Path d (e:es)) e'
  | target e == source e' = Path (d + weight e') (e':e:es)
  | otherwise = error "extend: Incompatible endpoints"

instance Edge e v => Edge (Path e) v where
  source (Path _ es) = source (last es)
  target (Path _ es) = target (head es)
  weight (Path w _)  = w

class Edge e v => Graph g e v | g -> e where
  vertices  :: g -> [v]
  edges     :: g -> [e]
  edgesFrom :: g -> v -> [e]
  edgesTo   :: g -> v -> [e]
  velem     :: v -> g -> Bool
  eelem     :: e -> g -> Bool

instance (Eq e, Edge e v) => Graph [e] e v where
  vertices es = nub (map source es ++ map target es)
  edges es    = es
  edgesFrom es v = [ e | e <- es, v == source e ]
  edgesTo   es v = [ e | e <- es, v == target e ]
  velem v es = v `elem` vertices es
  eelem v es = v `elem` edges es

instance Graph GameState (WormholeId, Wormhole) PlanetId where
  vertices (GameState ps _ _) = M.keys ps
  edges    (GameState _ ws _) = M.assocs ws
  edgesTo   st pId = M.toList (wormholesTo (Target pId) st)
  edgesFrom st pId = M.toList (wormholesFrom (Source pId) st)
  velem pId      (GameState ps _ _) = M.member pId ps
  eelem (wId, _) (GameState _ ws _) = M.member wId ws

data WormholeWithId = WormholeWithId WormholeId Wormhole
data Heap a = Heap (a -> a -> Ordering) (Tree a)
data Tree a = Nil | Node Int (Tree a) a (Tree a)
newtype AdjList e v = AdjList [(v, [e])]

rankTree :: Tree a -> Int
rankTree Nil            = 0
rankTree (Node h l x r) = h

node :: Tree a -> a -> Tree a -> Tree a
node l x r
  | hl < hr   = Node (hl + 1) r x l
  | otherwise = Node (hr + 1) l x r
 where
  hl = rankTree l
  hr = rankTree r

mergeHeap :: Heap a -> Heap a -> Heap a
mergeHeap (Heap cmp l) (Heap _ r) = Heap cmp (mergeTree cmp l r)

mergeTree
  :: (a -> a -> Ordering) -> Tree a -> Tree a -> Tree a
mergeTree cmp Nil r = r
mergeTree cmp l Nil = l
mergeTree cmp left@(Node _ l lx r) right@(Node _ _ rx _)
  | lte cmp lx rx = node l lx (mergeTree cmp r right)
  | otherwise     = mergeTree cmp right left

instance PQueue Heap where
  priority :: Heap a -> (a -> a -> Ordering)
  priority (Heap cmp t) = cmp

  empty :: (a -> a -> Ordering) -> Heap a
  empty cmp = Heap cmp Nil

  isEmpty :: Heap a -> Bool
  isEmpty (Heap _ Nil) = True
  isEmpty _ = False

  insert :: a -> Heap a -> Heap a
  insert val h@(Heap cmp _) = mergeHeap (Heap cmp (node Nil val Nil)) h

  extract :: Heap a -> a
  extract (Heap _ (Node _ _ x _)) = x
  extract _ = error "extract: empty heap!"

  discard :: Heap a -> Heap a
  discard h@(Heap cmp (Node _ l _ r)) = mergeHeap (Heap cmp l) (Heap cmp r)
  discard _ = error "discard: empty heap!"

instance Edge WormholeWithId PlanetId where
  source (WormholeWithId _ w) = source w
  target (WormholeWithId _ w) = target w
  weight (WormholeWithId _ w) = weight w

instance Eq WormholeWithId where
  (==) (WormholeWithId id1 _) (WormholeWithId id2 _) = id1 == id2

instance (Eq e, Edge e v) => Graph (AdjList e v) e v where
  vertices :: (AdjList e v) -> [v]
  vertices (AdjList ves)    = map fst ves

  edges :: (AdjList e v) -> [e]
  edges (AdjList ves)       = concat (map snd ves)

  edgesFrom :: (AdjList e v) -> v -> [e]
  edgesFrom (AdjList ves) s = fromJust (lookup s ves)

  edgesTo :: (AdjList e v) -> v -> [e]
  edgesTo g t               = filter (\x -> target x == t) (edges g)

  velem :: v -> (AdjList e v) -> Bool
  velem v g                 = elem v (vertices g)

  eelem :: e -> (AdjList e v) -> Bool
  eelem e g                 = elem e (edges g)

lt :: (a -> a -> Ordering) -> (a -> a -> Bool)
lt cmp x y = cmp x y == LT

gt :: (a -> a -> Ordering) -> (a -> a -> Bool)
gt cmp x y = cmp x y == GT

lte :: (a -> a -> Ordering) -> (a -> a -> Bool)
lte cmp x y = cmp x y /= GT

eq :: (a -> a -> Ordering) -> (a -> a -> Bool)
eq cmp x y = cmp x y == EQ

class PQueue pqueue where
  toPQueue   :: (a -> a -> Ordering) -> [a] -> pqueue a
  toPQueue cmp xs = foldr insert (empty cmp) xs
 
  fromPQueue :: pqueue a -> [a]
  fromPQueue = unfoldr unqueue
      where
        unqueue q
          | isEmpty q = Nothing
          | otherwise = Just (detach q)

  priority :: pqueue a -> (a -> a -> Ordering)

  empty :: (a -> a -> Ordering) -> pqueue a
  isEmpty :: pqueue a -> Bool

  insert :: a -> pqueue a -> pqueue a

  extract :: pqueue a -> a
  discard :: pqueue a -> pqueue a
  detach  :: pqueue a -> (a, pqueue a)
  detach q = (extract q, discard q)

data PList a = PList (a -> a -> Ordering) [a]

instance PQueue PList where

  toPQueue cmp xs = PList cmp (sortBy cmp xs)

  fromPQueue (PList _ xs) = xs

  empty cmp = PList cmp []

  isEmpty (PList _ xs) = null xs

  priority (PList cmp _) = cmp

  insert x (PList cmp []) = PList cmp [x]
  insert x ps@(PList cmp xs)
    | x <= y    = cons x ps
    | otherwise = cons y (insert x ys)
    where (<=) = lte cmp
          (y, ys) = detach ps
          cons x (PList cmp xs) = PList cmp (x:xs)

  extract (PList cmp (x:xs)) = x

  discard (PList cmp (x:xs)) = PList cmp xs

cmpPath :: Path v -> Path v -> Ordering
cmpPath (Path d _) (Path d' _) = compare d d'

shortestPaths :: forall g e v. Graph g e v => g -> v -> [Path e]
shortestPaths g v = dijkstra g (vertices g \\ [v]) ps
 where
  ps :: PList (Path e)
  ps = toPQueue cmpPath (map pathFromEdge (edgesFrom g v))

dijkstra :: (Graph g e v, PQueue pqueue) =>
  g -> [v] -> pqueue (Path e) -> [Path e]
dijkstra g [] ps = []
dijkstra g us ps
  | isEmpty ps  = []
  | t `elem` us = p : dijkstra g (us \\ [t])
                                 (foldr insert ps' (map (extend p) (edgesFrom g t)))
  | otherwise  = dijkstra g us ps'
  where
    (p, ps') = detach ps
    t = target p

lookupPlanet :: PlanetId -> GameState -> Planet
lookupPlanet pId (GameState ps _ _) = fromJust (M.lookup pId ps)

targetPlanets :: GameState -> Source -> [(PlanetId, Ships, Growth)]
targetPlanets st s
  = map (planetDetails . target) (M.elems (wormholesFrom s st))
    where
      planetDetails :: PlanetId -> (PlanetId, Ships, Growth)
      planetDetails pId = (pId, ships, growth)
        where Planet _ ships growth = lookupPlanet pId st

shipsOnPlanet :: GameState -> PlanetId -> Ships
shipsOnPlanet st pId = ships
  where Planet _ ships _ = lookupPlanet pId st

bknapsack'' :: forall name weight value .
  (Ord name, Ix weight, Ord weight, Num weight, 
    Ord value, Num value) =>
  [(name, weight, value)] -> weight -> (value, [name])
bknapsack'' wvs c = table ! (length wvs, c)
  where table :: Array (Int, weight) (value, [name])
        table = tabulate ((0, 0), (length wvs, c)) mbknapsack

        mbknapsack :: (Int, weight) -> (value, [name])
        mbknapsack (i, c)
          | i == 0    = (0, [])
          | c - w >= 0 = maxBy fst excluded included
          | otherwise = excluded
            where excluded   = table ! (i-1, c)
                  included   = (v + maxV, n : ns)
                  (n,w,v)    = wvs !! (i-1)
                  (maxV, ns) = table ! (i-1, c-w)

optimise :: GameState -> Source -> (Growth, [PlanetId])
optimise st s@(Source p)
  = bknapsack'' (filter (\(pId, _, _) -> let (Planet owner _ _) = lookupPlanet pId st in owner /= Owned Player1) (targetPlanets st s)) ((shipsOnPlanet st p))

shortestPaths' :: forall g e v . Graph g e v => g -> v -> [Path e]
shortestPaths' g v = dijkstra g (vertices g \\ [v]) ps
  where
    ps :: Heap (Path e)
    ps = foldr Lib.insert (empty cmpPath) (map pathFromEdge (edgesFrom g v))

conflictZones :: GameState -> PlanetId -> PlanetId
  -> ([PlanetId], [PlanetId], [PlanetId])
conflictZones st p q = conflictZones' vs pReachPair qReachPair
  where
    pvs = map fst pReachPair
    qvs = map fst qReachPair
    pReachPair = [ (target (head es), l) | (Path l es) <- shortestPaths' (AdjList ves) p ]
    qReachPair = [ (target (head es), l) | (Path l es) <- shortestPaths' (AdjList ves) q ]
    ves       = zip vs [ map (\(id, wh) -> WormholeWithId id wh) (edgesFrom st v) | v <- vs ]
    vs        = vertices st

    conflictZones' :: [PlanetId] -> [(PlanetId, Weight)] -> [(PlanetId, Weight)] -> ([PlanetId], [PlanetId], [PlanetId])
    conflictZones' [] _ _  = ([], [], [])
    conflictZones' (x:xs) plist qlist
      | not (elem x pvs || elem x qvs) = res
      | not (elem x pvs)               = (ps, x:qs, pqs)
      | not (elem x qvs)               = (x:ps, qs, pqs)
      | pw < qw   = (x:ps, qs, pqs)
      | pw > qw   = (ps, x:qs, pqs)
      | otherwise = (ps, qs, x:pqs)
        where pw = fromJust (lookup x plist)
              qw = fromJust (lookup x qlist)
              res@(ps, qs, pqs) = conflictZones' xs plist qlist

deriving instance Eq Player
deriving instance Show Player
deriving instance Read Player
deriving instance Generic Player
instance B.Binary Player
deriving instance Eq Owner
deriving instance Show Owner
deriving instance Read Owner
deriving instance Generic Owner
instance B.Binary Owner
deriving instance Show Planet
deriving instance Read Planet
deriving instance Generic Planet
instance B.Binary Planet
deriving instance Show Fleet
deriving instance Read Fleet
deriving instance Generic Fleet
instance B.Binary Fleet

deriving instance Show Wormhole
deriving instance Read Wormhole
deriving instance Generic Wormhole
instance B.Binary Wormhole

deriving instance Show Order
deriving instance Read Order
deriving instance Generic Order
instance B.Binary Order
deriving instance Show GameState
deriving instance Read GameState
deriving instance Generic GameState
instance B.Binary GameState

deriving instance Ord PlanetId
deriving instance Eq PlanetId
deriving instance Num PlanetId
deriving instance B.Binary PlanetId
instance Show PlanetId where
  show (PlanetId x) = show x
instance Read PlanetId where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ord Turns
deriving instance Eq Turns
deriving instance Num Turns
deriving instance B.Binary Turns
instance Show Turns where
  show (Turns x) = show x
instance Read Turns where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ord Source
deriving instance Eq Source
deriving instance B.Binary Source
instance Show Source where
  show (Source x) = show x
instance Read Source where
  readsPrec = coerce (readsPrec @Int)

deriving instance Num Growth
deriving instance Ord Growth
deriving instance Eq Growth
deriving instance B.Binary Growth
instance Show Growth where
  show (Growth x) = show x
instance Read Growth where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ix Ships
deriving instance Num Ships
deriving instance Ord Ships
deriving instance Eq Ships
deriving instance B.Binary Ships
instance Show Ships where
  show (Ships x) = show x
instance Read Ships where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ord Target
deriving instance Eq Target
deriving instance B.Binary Target
instance Show Target where
  show (Target x) = show x
instance Read Target where
  readsPrec = coerce (readsPrec @Int)

deriving instance Eq WormholeId
deriving instance Ord WormholeId
deriving instance B.Binary WormholeId
instance Show WormholeId where
  show (WormholeId x) = show x
instance Read WormholeId where
  readsPrec = coerce (readsPrec @Int)

deriving instance Eq e   => Eq (Path e)
deriving instance Read e => Read (Path e)
deriving instance Show e => Show (Path e)
instance Show a => Show (PList a) where
  show (PList _ xs) = show xs

deriving instance Generic PlanetId
deriving instance Generic WormholeId
deriving instance Generic Ships
deriving instance Generic (Path a)
 
instance NFData PlanetId
instance NFData Order
instance NFData WormholeId
instance NFData Ships
instance NFData a => NFData (Path a)
