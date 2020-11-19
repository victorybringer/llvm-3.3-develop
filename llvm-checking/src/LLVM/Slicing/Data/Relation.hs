{-# LANGUAGE FlexibleInstances,FlexibleContexts,MonoLocalBinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Relation.SetOfPairs
-- Copyright   :  (c) Joost Visser 2004
-- License     :  LGPL
-- 
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of relations as sets of pairs.
--
-----------------------------------------------------------------------------

module LLVM.Slicing.Data.Relation  where

import qualified Data.Set as Set
import Data.Set(Set) 
import qualified Data.Map as Map  hiding (showTreeWith)
import qualified Data.Map.Internal.Debug as Map (showTreeWith)
import Data.Map  (Map) 

import System.Process (system) 
import Data.Ratio
import Data.List (transpose,(\\)) 
import Control.Monad.State

-- zyz
import qualified Data.Graph.Inductive as G
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM 
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS


-----------------------------------------------------------------------------
-- * Slicing and chopping

-- | Compute forward slice starting from seeds.
slice            :: Ord a => Set a -> Rel a a -> Rel a a
slice seed r        = sliceUntil seed Set.empty r

-- | Compute forward slice starting from seeds, stopping at stoppers.
sliceUntil        :: Ord a => Set a -> Set a -> Rel a a -> Rel a a
sliceUntil seed stop r    
                = if (Set.null s) then emptyRel 
                                    else r' `Set.union`
                                         (sliceUntil seed' stop' r)
              where
                s = seed `Set.difference` stop    -- live seeds
                r' = project s r            -- step
                seed' = rng r'              -- new seeds
                stop' = stop `Set.union` s      -- extend stoppers

-- | Compute backward slice starting from seeds.
sliceBackward        :: Ord a => Set a -> Rel a a -> Rel a a
sliceBackward seed r    = inv (slice seed (inv r))

-- | Compute chop between seeds and sinks.
chop                    :: Ord a => Set a -> Set a -> Rel a a -> Rel a a
chop seeds sinks r  = slicesWith Set.intersection seeds sinks r
                      
-- | Compute union of backward and forward slice.
slicesUnion :: Ord a => Set a -> Set a -> Rel a a -> Rel a a
slicesUnion seeds sinks r
  = (slice seeds r) `Set.union` (sliceBackward sinks r)

-- | Compute intersection of backward and forward slice.
--   This is the same as the computing the chop between seeds and sinks.
slicesIntersect :: Ord a => Set a -> Set a -> Rel a a -> Rel a a
slicesIntersect seeds sinks r  
  = (slice seeds r) `Set.intersection` (sliceBackward sinks r)
   
-- | Compute combination of backward and forward slice, where
--   a binary operator is supplied to specify the kind of combination.
slicesWith :: Ord a 
           => (Rel a a -> Rel a a -> Rel a a) 
           -> Set a -> Set a -> Rel a a -> Rel a a
slicesWith binop seeds sinks r
  = (slice seeds r) `binop` (sliceBackward sinks r)

-- | Compute slice or chop, depending on whether the source or sink set or both 
--   are empty.
sliceOrChopWith :: (Ord a) 
                => (Rel a a -> Rel a a -> Rel a a)  -- ^ binary graph operation
                -> [a]                              -- ^ sources
                -> [a]                              -- ^ sinks
                -> Rel a a                          -- ^ input relation
                -> Rel a a                          -- ^ output relation
sliceOrChopWith binop [] [] rel         = rel
sliceOrChopWith binop [] sinks rel      = sliceBackward (Set.fromList sinks) rel
sliceOrChopWith binop sources [] rel    = slice (Set.fromList sources) rel
sliceOrChopWith binop sources sinks rel 
  = slicesWith binop (Set.fromList sources) (Set.fromList sinks) rel
      
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- * Representation

-- | Type of relations
type Rel a b = Set (a,b)

-----------------------------------------------------------------------------
-- * Building relations           

-- | Build an empty relation.              
emptyRel        :: Rel a b
emptyRel        = Set.empty

-- | Build a relation from a list of pairs.
mkRel           :: (Ord a, Ord b) => [(a,b)] -> Rel a b
mkRel pairs     = Set.fromList pairs

-- | Build a relation from distributing an element to a set of elements
mkRelNeighbors      :: (Ord a, Ord b) => a -> [b] -> Rel a b
mkRelNeighbors a l  = Set.fromList [ (a, x) | x <- l ]

-- | Build identity relation, which contains an edge from each node to itself.
identityRel      :: Ord a => Set a -> Rel a a
identityRel s    = Set.map (\x -> (x,x)) s

-- | Build total relation, which contains an edge from each node to 
--   each other node and to itself.
totalRel         :: Ord a => Set a -> Rel a a
totalRel s       = Set.fromList [ (x,y) |  x <- l, y <- l ]
                   where
                     l = Set.elems s

-- | Build a chain relation of given number of numerals.
chainRel        :: (Enum n, Num n, Ord n) =>  n -> Rel n n
chainRel n      = Set.fromList (Prelude.map (\i -> (i,i+1)) [1..n])

-- | Build a relation from a predicate
predRel :: Ord a => Set a -> (a -> Bool) -> Rel a a
predRel a p = identityRel $ Set.filter p a 

-- | Build a relation (correflexive) from a set
setRel :: Ord a => Set a -> Rel a a
setRel s = identityRel s

-----------------------------------------------------------------------------
-- * Basic operations

-- | Obtain the domain of a relation
dom             :: (Ord a, Ord b) => Rel a b -> Set a
dom xs          = Set.map fst xs

-- | Obtain the range of a relation
rng             :: (Ord a, Ord b) => Rel a b -> Set b
rng xs          = Set.map snd xs

-- | Obtain the subdomain of a relation given a subrange.
domWith             :: (Ord a, Ord b) => Set b -> Rel a b -> Set a
domWith bs r        = Set.fromList [ a | (a,b) <- Set.elems r , b `Set.member` bs ]

-- | Obtain the subrange of a relation given a subdomain.
rngWith             :: (Ord a, Ord b) => Set a -> Rel a b -> Set b
rngWith as r        = Set.fromList [ b | (a,b) <- Set.elems r , a `Set.member` as ]

-- | Convert relation to a list of pairs.
pairs           :: (Ord a, Ord b) => Rel a b -> [(a,b)]
pairs r         = Set.elems r

-- | Take the inverse of a relation
inv            :: (Ord a, Ord b) => Rel a b -> Rel b a
inv xs         = Set.map (\(x,y) -> (y,x)) xs

-- | Compose two relations
comp            :: (Ord a, Eq b, Ord c) => Rel b c -> Rel a b -> Rel a c
comp yz xy      = Set.fromList 
                    [ (x,z) | (x,y) <- Set.elems xy
                            , (y',z) <- Set.elems yz
                            , y==y'
                    ]

-- | Kernel of a relationship.
ker :: (Ord a, Ord b) => Rel a b -> Rel a a
ker r = inv r `comp` r

-- | Image of a relationaship.
img :: (Ord a, Ord b) => Rel a b -> Rel b b
img r = r `comp` inv r


-- http://www.ics.uci.edu/~eppstein/161/960220.html
    
-- type b :<-: a = Rel a b
-- type a :->: b = Rel a b

-----------------------------------------------------------------------------
-- * Projection

-- | Retrieve a subrelation given predicates on domain and range.
projectWith :: (Ord a, Ord b) 
            => (a -> b -> Bool) 
            -> Rel a b -> Rel a b
projectWith p = Set.filter (uncurry p)

-- | Projection of set through relation
project     :: (Ord a, Ord b) => Set a -> Rel a b -> Rel a b
project s    = Set.filter ( \(x,_) -> Set.member x s ) 

-- | Projection of set backward through relation
projectBackward    :: (Ord a, Ord b) => Set b -> Rel a b -> Rel a b
projectBackward s   = Set.filter ( \(_,y) -> Set.member y s )

-----------------------------------------------------------------------------
-- | The type of labeled relations.
type LRel a b c  = Map (a,b) c


-- | Convert labeled relation to one without labels. The first argument
--   is a predicate that determines, based on the label, which pairs are
--   to be included in the result relation.
rel         :: (Ord a, Ord b) 
            => (c -> Bool)       -- ^ Predicate on labels.
            -> LRel a b c 
            -> Rel a b
rel p fm    = Set.fromList (Map.keys (Map.filter p fm))

-- | Convert relation to a labeled one. The first argument is the 
--   label with which all pairs in the result relation will be labeled.
lrel        :: (Ord a, Ord b) 
            => c                -- ^ Initialization label
            -> Rel a b 
            -> LRel a b c
lrel c r    = Map.fromList (zip (pairs r) (repeat c)) 

-- | Carrier set.
entities    :: (Ord a) => LRel a a c -> Set a
entities lr = Set.fromList ((map fst pairs) ++ (map snd pairs))
              where
                pairs = Map.keys lr  

-- | Label set.
labels      :: (Ord a, Ord b, Ord c) => LRel a b c -> Set c
labels lr   = Set.fromList $ Map.elems lr     

-----------------------------------------------------------------------------
-- * Extremal paths.

-- | Generic extremal path algorithm, based on Roland Backhouse's 
--   lecture notes, page 192 (draft version).
extremal :: Ord a 
            => b                -- ^ Zero of multiplication.
            -> (b -> b -> b)    -- ^ Multiplication. 
            -> (b -> b -> b)    -- ^ Addition.
            -> LRel a a b -> LRel a a b
extremal zero and or lr
            = foldr step lr nodes
              where
                nodes         = Set.elems (entities lr)
                step k lr     = mk [((i,j), value i j k lr) 
                                             | i <- nodes
                                             , j <- nodes 
                                   ]
                value i j k lr 
                              = (p lr (i,j)) 
                                `or` ( (p lr (i,k))
                                     `and` (p lr (k,j)) )
                mk            = Map.fromList
                p lr (i,j)    = Map.findWithDefault zero (i,j) lr

-- | Reachability. This is transitive closure following Roy-Warshall.
reach       :: Ord a => LRel a a Bool -> LRel a a Bool
reach r     = extremal False (&&) (||) r

-- | Least cost, or shortest path. 
leastcost       :: (Num cost, Ord cost, Ord a) 
                => cost               -- ^ Maximum bound. 
                -> LRel a a cost      -- ^ Cost labels must be non-negative.
                -> LRel a a cost
leastcost maxBound r 
                = extremal maxBound (+) min r

-- | Worst cost, or longest path. UNTESTED.
worstcost       :: (Ord a, Num cost, Ord cost) 
                => cost
                -> LRel a a cost
                -> LRel a a cost
worstcost maxBound r   
                = extremal maxBound (+) max r

-- | Bottle neck.
bottleneck      :: (Ord height, Num height, Ord a) 
                => LRel a a height      -- ^ Height labels must be non-negative.
                -> LRel a a height
bottleneck r    = extremal 0 min max r

-- | Least cost, or shortest path. DOES NOT WORK, because it relies on 
--   maxBound from the Bounded class, leading to erroneous arithmetic.
leastcost'      :: (Num cost, Ord cost, Bounded cost, Ord a) 
                => LRel a a cost -> LRel a a cost
leastcost' r    = extremal maxBound (+) min r

-- | Not quite right when cycles are present.
height1 :: Ord a => Rel a a -> Integer
height1 g = Map.foldr' (\a b -> maybe b (max b) a) 0 $ heights 
  where
    heights = extremal zero and or lr
    lr = lrel (Just 2) g
    zero = Nothing
    and (Just x) (Just y) = Just (x+y-1)
    and _ _ = Nothing
    or (Just x) (Just y) = Just $ max x y
    or (Just x) Nothing  = Just $ x
    or Nothing (Just y)  = Just $ y
    or _ _               = Nothing
    

-----------------------------------------------------------------------------
--- * Graphs 

-- | Type of graphs: relations with domain and range of the same type.
type Gph a = Rel a a       

-- | Type of graphs, with explicit entity set.
type Graph a = (Set a, Gph a)

-----------------------------------------------------------------------------
-- * Reductions

-- | Reflexive reduction, i.e remove self-edges.
reflReduc :: Ord a => Gph a -> Gph a
reflReduc g = Set.filter (\(x,y) -> x /= y) g

-----------------------------------------------------------------------------
-- * Basic graph queries

-- | Find top nodes of a graph. Those nodes that have outgoing but no
--   incoming edges. 
topNodes :: Ord a => Gph a -> Set a
topNodes g = dom g `Set.difference` rng g

-- | Find bottom nodes of a graph. Those nodes that have incoming but no
--   outgoing edges. 
bottomNodes :: Ord a => Gph a -> Set a
bottomNodes g = rng g `Set.difference` dom g

-- | Find internal nodes of a graph. Those nodes that have both 
--   incoming and outgoing edges. 
internalNodes :: Ord a => Gph a -> Set a
internalNodes g = rng g `Set.intersection` dom g

-- | Remove all edges of the form (x,x).
removeSelfEdges :: Ord a => Gph a -> Gph a
removeSelfEdges r
  = mkRel [ (x,y) | (x,y) <- Set.elems r, x /= y ]

-- | Obtain all entities in a relation (union of domain and range)
ent             :: Ord a => Rel a a -> Set a
ent xs          = dom xs `Set.union` rng xs


-- | Obtain the subrange and subdomain of a relation given a subdomain.
entWith             :: Ord a => Set a -> Rel a a -> Set a
entWith as r        = domWith as r `Set.union` rngWith as r

-----------------------------------------------------------------------------
-- * Costs with infinity

-- | Compute depth of a graph.
depth           :: (Ord a) => Gph a -> Int
depth r         = foldr (\d a -> maybe a (max a) d) 0 $ 
                   Set.elems $ labels $ 
                    extremal infDepth addDepth maxDepth $ 
                      lrel (mkDepth (1::Int)) r
  where
   -- We model depth as (Maybe Int), where Nothing denotes
   -- infinite cost.
   infDepth = Nothing
   mkDepth n = Just n
   maxDepth Nothing d = d
   maxDepth d Nothing = d
   maxDepth (Just d) (Just d') = Just (max d d')
   addDepth Nothing d = Nothing
   addDepth d Nothing = Nothing
   addDepth (Just d) (Just d') = Just (d + d')

-----------------------------------------------------------------------------
-- * Weak components

-- | Compute subrelation connected to seeds, stopping at stoppers.
--  In fact, gobbleUtil is similar to slice except at the step.
gobbleUntil        :: Ord a => Set a -> Set a -> Gph a -> Gph a
gobbleUntil seed stop r    
  = if (Set.null s) 
      then emptyRel 
      else r' `Set.union` (gobbleUntil seed' stop' r)
    where
      s = seed `Set.difference` stop                         -- live seeds
      r' = (project s r) `Set.union` (projectBackward s r)   -- step
      seed' = ent r'                                         -- new seeds
      stop' = stop `Set.union` s                             -- extend stoppers

-- | Compute weakly connected components.
weakComponents :: Ord a => Gph a -> [Gph a]
weakComponents r
  = weakComponentsUntil Set.empty r
    where
      weakComponentsUntil done r
        = case Set.elems ((ent r) `Set.difference` done) of
           []    -> []
           (a:_) -> (r':s)
                    where
                      r' = gobbleUntil (Set.singleton a) Set.empty r
                      s  = weakComponentsUntil (done `Set.union` (ent r')) r   

-- | Compute weakly connected components (sets of nodes)
weakComponentSet :: Ord a => Gph a -> Set (Set a)
weakComponentSet r
 = Set.fromList (Prelude.map ent $ weakComponents r)

-----------------------------------------------------------------------------
-- * Strong components

-- ** Components as sets of nodes

-- | Compute a single strong component (set of nodes).
strongComponent :: (Ord a) => a -> Gph a -> Set a
strongComponent x r
  = Set.insert x reach
    where
      reach = (reachableFrom x r) `Set.intersection` (reachableFrom x (inv r))
      reachableFrom x r
        = ent (slice (Set.singleton x) r)
        
-- | Compute a single strong component (set of nodes).
strongComponentExcluding :: (Ord a) => Set a -> a -> Gph a -> Set a
strongComponentExcluding done x r
  = Set.insert x reach
    where
      reach = (reachableFrom x r) `Set.intersection` (reachableFrom x (inv r))
      reachableFrom x r
        = ent (sliceUntil (Set.singleton x) done r)
        
-- | Compute the set of strong components (sets of nodes).
--   Not optimized.
strongComponentSet :: (Ord a, Ord (Set a)) => Gph a -> Set (Set a)
strongComponentSet r
  = Set.map (\x -> strongComponent x r) (ent r)
  
{-
-- | Compute the graph of strong components (node sets).
strongComponentGraph :: (Ord a, Ord (Set a)) => Gph a -> Graph (Set a) 
strongComponentGraph r
  = (st, removeSelfEdges gph)
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = findWithDefault (error "SCC map misses entry") x mp
      gph = Set.map edgeT r
      st = Set.fromList $ elems mp
      -- Using finite map from nodes to SSCs for performance reasons:
      mp = fromList $ Prelude.map (\x -> (x, strongComponent x r))  $ Set.elems $ ent r
-}

-- | Compute the graph of strong components (node sets).
strongComponentGraph :: (Ord a, Ord (Set a)) => Gph a -> Graph (Set a) 
-- strongComponentGraph r = componentGraph strongComponent r
strongComponentGraph r = componentGraph' strongComponentExcluding id r

componentGraph 
  :: (Ord node, Ord comp)
  => (node -> Gph node -> comp) 
  -> Gph node 
  -> Graph comp
componentGraph mkComponent r
  = (st,gph)
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = Map.findWithDefault (error "SCC map misses entry") x mp
      gph = Set.fold worker Set.empty r
      worker (x,y) g = insertNonSelfEdge (nodeT x) (nodeT y) g
      insertNonSelfEdge xc yc g = if xc==yc then g else Set.insert (xc,yc) g
      st = Set.fromList $ Map.elems mp
      -- Using finite map from nodes to components for performance reasons:
      mp = Set.fold (\x -> Map.insert x (mkComponent x r)) Map.empty (ent r)

componentGraph' 
  :: (Ord node, Ord comp)
  => (Set node -> node -> Gph node -> comp) 
  -> (comp -> Set node)
  -> Gph node 
  -> Graph comp
componentGraph' mkComponent getEnt r
  = (st,gph)
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = Map.findWithDefault (error "SCC map misses entry") x mp
      gph = Set.fold worker Set.empty r
      worker (x,y) g = insertNonSelfEdge (nodeT x) (nodeT y) g
      insertNonSelfEdge xc yc g = if xc==yc then g else Set.insert (xc,yc) g
      st = Set.fromList $ Map.elems mp
      -- Using finite map from nodes to components for performance reasons:
      (mp,_) = Set.fold addCmp (Map.empty,Set.empty) (ent r)
      addCmp x (mp,done) 
        | x `Set.member` done = (mp,done)
        | otherwise           = (mp',done')
        where
          cmp = mkComponent done x r
          entcmp = getEnt cmp
          mp' = Set.fold (\x -> Map.insert x cmp) mp entcmp
          done' = done `Set.union` entcmp

-- | Compute the set of strong components that have more than a single node
strongNonSingletonComponentSet :: (Ord a, Ord (Set a)) 
                               => Gph a -> Set (Set a)
strongNonSingletonComponentSet
  = Set.filter (\c -> Set.size c > 1) . strongComponentSet

-- ** Components as graphs

-- | Type of components: node set tupled with subgraph.
type Component a = Graph a

-- | Compute a single strong component (node set tupled with subrelation).
strongComponent' :: (Ord a) => a -> Rel a a -> Component a
strongComponent' x r
  = (scc,subrel)
    where
      scc = strongComponent x r
      subrel = Set.filter (\(x,y) ->  x `Set.member` scc && y `Set.member` scc ) r

strongComponentExcluding' :: (Ord a) => Set a -> a -> Rel a a -> Component a
strongComponentExcluding' done x r
  = (scc,subrel)
    where
      scc = strongComponentExcluding done x r
      subrel = Set.filter (\(x,y) ->  x `Set.member` scc && y `Set.member` scc ) r

-- | Compute the graph of stong components (node sets tupled with subrelations).
strongComponentRel' :: (Ord a, Ord (Set a)) 
                    => Gph a -> Gph (Component a)
-- strongComponentRel' r = snd $ componentGraph strongComponent' r
strongComponentRel' r = snd $ componentGraph' strongComponentExcluding' fst r
{-
  = removeSelfEdges $ Set.map edgeT r
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = strongComponent' x r
-}

{- Inefficient, use (snd $ strongComponentGraph) instead.
-- | Compute the graph of strong components (node sets).
--   Note: strong components that have no relations to other components
--   will not be present in the graph!
strongComponentRel :: (Ord a, Ord (Set a)) => Gph a -> Gph (Set a) 
strongComponentRel r
  = removeSelfEdges $ Set.map edgeT r
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = (strongComponent x r)
-}

-----------------------------------------------------------------------------
-- * Othter graph partitionings

-- | Derive a graph of groups, based on a partitioning of the
--   nodes into groups.
partitionGraph :: Ord a => Set (Set a) -> Gph a -> Gph (Set a)
partitionGraph groups g
  = removeSelfEdges $ Set.map edgeT g
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = Map.findWithDefault (error "Group map misses entry") x mp
      mp = Set.fold worker Map.empty $ groups
      worker xs mp = Set.fold (\x mp -> Map.insert x xs mp) mp xs

-- | Split a graph into subgraphs, stored in a map with groups as keys.
subGraphs 
  :: Ord a 
  => Set (Set a)           -- ^ groups
  -> Gph a                 -- ^ graph
  -> Map (Set a) (Gph a)   -- ^ map from groups to corresponding subgraphs
subGraphs groups g = Set.fold worker Map.empty groups
  where
    worker group gs = Map.insert group (subGraph group g) gs

-- | Determine the subgraph of a given graph induced by a set of nodes.
subGraph :: Ord a => Set a -> Gph a -> Gph a
subGraph xs g = Set.fold worker emptyRel g
  where
    worker e@(x,y) g 
      | x `Set.member` xs && y `Set.member` xs = Set.insert e g
      | otherwise = g
 
-----------------------------------------------------------------------------
-- | Traversal (under construction)

dfs :: Ord a => Rel a a -> [a]
dfs r
  = dfsFrom r [] (Set.elems $ ent r)
    

dfsFrom r stop []
  = []
dfsFrom r stop (n:ns)
  | n `elem` stop
  = dfsFrom r stop ns
  | otherwise
  = fromKids++(n:(dfsFrom r stop'' ns))
    where
      kids = [ y | (x,y) <- Set.elems r, x==n ]
      stop' = n:stop
      fromKids = dfsFrom r stop' kids
      stop'' = stop'++fromKids
      
dfsFrom' r (stop,(n:ns))
  | n `elem` stop
  = dfsFrom' r (stop,ns)
  | otherwise
  = (stop''',fromKids++(n:fromRest))
    where
      kids = [ y | (x,y) <- Set.elems r, x==n ]
      stop' = n:stop
      (stop'',fromKids)  = dfsFrom' r (stop',kids)
      (stop''',fromRest) = dfsFrom' r (stop'',ns)
dfsFrom' r (stop,[])
  = (stop,[])
  
dfs',dfsS :: Ord a => Rel a a -> [a]            
dfs' r
  = snd $ dfsFrom' r ([],(Set.elems $ ent r))
   
dfsS r
  = evalState (dfsState r) ([],Set.elems $ ent r)
dfsState r  = do
  (stop,ns) <- get
  case ns of
    (n:ns)
      -> if (n `elem` stop)
          then do
            put (stop,ns)
            dfsState r
          else do
            let kids = [ y | (x,y) <- Set.elems r, x==n ]
            put (n:stop,kids)
            fromKids <- dfsState r
            (stop,_) <- get
            put (stop,ns)
            fromRest <- dfsState r
            return (fromKids++(n:fromRest))
    []
      -> return ns

dfsT :: Ord a => Rel a a -> Rel a a
dfsT r
  = evalState (dfsStateT r) ([],Set.elems $ ent r,emptyRel)
dfsStateT r  = do
  (stop,ns,t) <- get
  case ns of
    (n:ns)
      -> if (n `elem` stop)
          then do
            put (stop,ns,t)
            dfsStateT r
          else do
            let entT = ent t
            let edges = [ (x,y) | (x,y) <- Set.elems r, 
                                  x==n, not (y `Set.member` entT) ]
            let kids = map snd edges
            put (n:stop,kids,t `Set.union` (mkRel edges))
            dfsStateT r
            (stop,_,t) <- get
            put (stop,ns,t)
            dfsStateT r
    []
      -> return t


-----------------------------------------------------------------------------
-- * Integration

-- | Integrate two graphs with respect to a base graph into a
--   new graph that contains the differences of each input graph with
--   respect to the base graph. The boolean value indicates whether the
--   graphs are interference-free, i.e. whether the integration is valid. 
integrate :: Ord a => Rel a a -> Rel a a -> Rel a a -> (Rel a a,Bool)
integrate a b base
  = (m,ok)
    where
      apa = affectedPoints a base
      apb = affectedPoints b base
      aapa = a // apa
      bapb = b // apb
      basepp  = base // (preservedPoints a b base)
      m = aapa `Set.union` bapb `Set.union` basepp
      ok = m//apa == aapa && m//apb == bapb
      g / v = sliceBackward (Set.singleton v) g
      g // s = sliceBackward s g

-- | Points in the first graph that are affected by changes with respect
--   to the base graph.
affectedPoints :: Ord a => Rel a a -> Rel a a -> Set a
affectedPoints x base
        = Set.fromList [ v | v <- Set.elems (ent x), (base/v) /= (x/v) ]
          where
            g / v = sliceBackward (Set.singleton v) g

-- | Points in the base graph that are not affected by changes in either
--   input graph.
preservedPoints ::  Ord a => Rel a a -> Rel a a -> Rel a a -> Set a
preservedPoints x y base
        = Set.fromList [ v | v <- Set.elems (ent base), 
                      let  base' = base/v in base' == (x/v) && base' == (y/v)
                ]
          where
            g / v = sliceBackward (Set.singleton v) g


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * Bags

-- | The type of bags.
newtype Bag a = Bag {fm :: Map a Int} deriving Eq

-- | Add an element to a bag.
addToBag :: Ord a => Bag a -> a -> Bag a
addToBag Bag{ fm=bag } a
  = Bag (Map.insertWith (+) a 1 bag)

-- | Empty bag.
emptyBag :: Bag a
emptyBag = Bag Map.empty

-- | Put all elements from a list into a bag.
listToBag :: Ord a => [a] -> Bag a
listToBag xs
  = foldr (\x bag -> addToBag bag x) emptyBag xs

-- | Create the union of a list of bags.
unionBags :: Ord a => [Bag a] -> Bag a
unionBags bags = 
  Bag $ foldr (Map.unionWith (+)) Map.empty $ map fm bags
  
-- | Create the union of two bags.
unionBag :: Ord a => Bag a -> Bag a -> Bag a
unionBag b1 b2 = 
  Bag $ Map.unionWith (+) (fm b1) (fm b2)

-- | Return the frequency of the given element.
lookupBag :: Ord a => Bag a -> a -> Int 
lookupBag b a = Map.findWithDefault 0 a (fm b) 

-- | Fold over a bag.
foldBag :: (a -> Int -> u -> u) -> u -> Bag a -> u
foldBag f u (Bag m) = Map.foldrWithKey' f u m

------------------------------------------------------------------------------



-- | Given a set of sets ss, the resulting set is the union of all the elements
--   (these are sets themselves) of ss, i.e. it contains all the elements of all the 
--   sets of ss.
dunion :: Ord a => Set (Set a) -> Set a
dunion s = foldr (\s1 s2 -> Set.union s1 s2) Set.empty $ Set.toList s

-- | Applies a given function to a set read from a given file.  
readFile_Set :: (Read a, Ord a, Show c) => FilePath -> ((Set a) -> c) -> IO c
readFile_Set file f = readFile file >>= return . f . read


-- | Applies readFile_Set and writes the result in a given file.
interact_Set :: (Read a, Ord a, Show c) =>
         FilePath -> FilePath -> ((Set a) -> c) -> IO ()
interact_Set inn out f = readFile_Set inn f >>= writeFile out.show


readSet'  :: (Ord a,Read a) => ReadS (Set a)
readSet' str = Prelude.map (\(a,b)->(Set.fromList a,b)) (readSet str)

readSet   :: Read a => ReadS [a]
readSet   = readParen False (\r -> [pr | ("{",s) <- lex r,
                                         pr      <- readl s ])
            where readl  s = [([],t)   | ("}",t) <- lex s] ++
                             [(x:xs,u) | (x,t)   <- reads s,
                                         (xs,u)  <- readl' t]
                  readl' s = [([],t)   | ("}",t) <- lex s] ++
                             [(x:xs,v) | (",",t) <- lex s,
                                         (x,u)   <- reads t,
                                         (xs,v)  <- readl' u]
                                         
                                         
-------------------------------------------------------------------------------
-- | Create content for dot file representing the given relation.
printRel :: (Show a, Ord a, Show b, Ord b) 
           => GraphName -> Rel a b -> String
printRel graphName r
  = printRelWith show (const show) show (const show) graphName r
  

-- * Graph printing

-- | Print a graph to dot format. This function is parameterized 
--   with functions for printing names and labels of individual nodes.
printGraphWith 
  :: Ord a 
  => (a -> NodeName)              -- ^ print domain node identifier
  -> (Gph a -> a -> String)       -- ^ print domain node label
  -> GraphName                    -- ^ graph name
  -> Gph a                        -- ^ relation to print
  -> DotGraph                     -- ^ string to export to dot
printGraphWith showNode showNodeLabel graphName
  = printRelWith showNode showNodeLabel showNode showNodeLabel graphName

-- | Print a relation to dot format. This function is parameterized 
--   with functions for printing names and labels of individual nodes.
printRelWith 
  :: (Ord a, Ord b) 
  => (a -> NodeName)              -- ^ print domain node identifier
  -> (Rel a b -> a -> String)     -- ^ print domain node label 
  -> (b -> NodeName)              -- ^ print range node identifier
  -> (Rel a b -> b -> String)     -- ^ print range node label
  -> GraphName                    -- ^ graph name
  -> Rel a b                      -- ^ relation to print
  -> DotGraph                     -- ^ string to export to dot
printRelWith showInNode showInNodeLabel showOutNode showOutNodeLabel graphName r
  = "digraph "++graphName++" {\n"++
    unlines outNodeStats++
    unlines inNodeStats++
    unlines edgeStats++
    "}\n"
    where
      inNodeStats = map mkInNode (Set.elems (dom r))
      outNodeStats = map mkOutNode (Set.elems (rng r))
      edgeStats = map showEdge (Set.elems r)
      showEdge (x,y) = mkEdge "" (showInNode x, showOutNode y)
      mkInNode n = mkNode' ("shape=box,label="++ (quote (showInNodeLabel r n)))
                          (showInNode n)
      mkOutNode n = mkNode' ("shape=box,label="++ (quote (showOutNodeLabel r n)))
                           (showOutNode n)
      mkNode' "shape=box,label=\"\\n\"" n
        = mkNode "shape=circle,height=.2,label=\"\"" n
      mkNode' a n = mkNode a n
      
-- | Print a graph to dot format. This function is parameterized 
--   with functions for printing names and labels of individual nodes.
printComponentGraphWith 
  :: Ord a 
  => (a -> NodeName)              -- ^ print identifier for element of set
  -> (a -> String)                -- ^ pretty-print element of set
  -> GraphName                    -- ^ graph name
  -> Gph (Set a)                  -- ^ relation to print
  -> DotGraph                     -- ^ string to export to dot
printComponentGraphWith showNodeName showNodeLabel graphName
  = printGraphWith showSet (\_ x -> showFormattedSet showNodeLabel x) graphName
    where
      showSet s    = show' $ Set.elems s
      show' []     = ""
      show' (x:xs) = showNodeName x ++ showl xs
      showl []     = ""
      showl (x:xs) = "," ++ showNodeName x ++ showl xs

-- | Function that shows the elemens of a set spread nicely
--   over lines that are not too long. On each line elements
--   are separated by spaces. The number of elements of the
--   set determines how long each line is allowed to grow.
showFormattedSet 
  :: (a -> String)    -- ^ show function for elements
  -> Set a            -- ^ set to show
  -> String 
showFormattedSet showNode xs = shows 0 lst
  where
    lst = Set.elems xs
    max = Set.size xs
    mx = maximum (max:(map (length.showNode) lst))
    shows l []      = ""
    shows 0 (x:xs)  = wrd++(shows (length wrd) xs) where wrd = showNode x
    shows l (x:xs) 
      | l' > mx   = '\\':'n':(shows 0 (x:xs))
      | otherwise  = ' ':wrd++(shows l' xs)
      where 
        l' = l+(length wrd)+1
        wrd = showNode x

--------------------------
-- | The type of dot statements.  
type DotStatement = String

-- | A list of dot attributes is represented by a string.
type DotAttributes = String

-- | The type of node names
type NodeName = String

-- | The type of graph names
type GraphName = String

-- | The type of exportable graphs (file content)
type DotGraph = String

-------------------------------------------------------------------------------
-- * Printing

-- | Create a node statement
mkNode :: DotAttributes -> NodeName -> DotStatement
mkNode as ""        = "" 
mkNode as n         = quote n++" [ "++as++" ]" 

-- | Create an edge statement
mkEdge :: DotAttributes -> (NodeName,NodeName) -> DotStatement
mkEdge as ("",y)    = "" 
mkEdge as (x,y)     = quote x++" -> "++quote y++" [ "++as++" ]" 

-- | Create an edge statement with inverted direction
mkEdgeBack :: DotAttributes -> (NodeName,NodeName) -> DotStatement
mkEdgeBack as ("",y)    = "" 
mkEdgeBack as (x,y)     = quote y++" -> "++quote x++" [ "++as++" ]" 

-- * Auxilliaries

-- | Put quotes around a string.
quote :: String -> String
quote s = "\""++(esc s)++"\""
  where
    esc [] = []
    esc ('\"':cs) = '\\':'\"':esc cs
    esc (c:cs) = c:esc cs

-------------------------------------------------------------------------------
-- zyz
printGr filename contents = do
   writeFile dotfile contents
   system $ "dot -Tpng " ++ dotfile ++ " -o " ++ pngfile
   return ()
  where 
   dotfile = unsuffix filename ++ ".dot"
   pngfile = unsuffix filename ++ ".png"
   unsuffix = reverse.tail.dropWhile (/= '.').reverse    

-- 
printRelTwo graphName r1 r2
  = "digraph " ++ graphName ++ " {\n" ++
    unlines nodes ++
    unlines dataEdges ++
    unlines ctrlEdges ++
    "}\n"
   where
      nodes = map (mkNode "" . show) (Set.elems $ ent r1 `Set.union` ent r2)
      dataEdges = map showDEdge (Set.elems r1)
      ctrlEdges = map showCEdge (Set.elems r2)
      showDEdge (x,y) = mkEdge "" (show x, show y)
      showCEdge (x,y) = mkEdge "color=\".8 .7 .8\" style=dashed" (show x, show y)

printRelSlice vars r r2
  = "digraph " ++ "DynamicSlice" ++ " {\n" ++
    unlines varNodes ++ unlines sliceNodes ++ unlines otherNodes ++
    unlines sliceEdges ++ unlines otherEdges ++
    "}\n"
   where
      varNodes = map (mkNode "style=filled, color=\"1 1 1\"" . show) vars
      sliceNodes = map (mkNode "style=filled, color=\".7 .3 1.0\"". show)
                       $ (Set.elems $ ent r2) \\ vars
      otherNodes = map (mkNode "" . show) $ (Set.elems $ ent r Set.\\ ent r2) \\ vars
      sliceEdges = map showSEdge (pairs r2)
      otherEdges = map showOEdge (pairs r \\ pairs r2)
      showOEdge (x,y) = mkEdge "" (show x, show y)
      showSEdge (x,y) = mkEdge "color=\"1 1 1\"" (show x, show y)      

-- another printRel function which I use for the most of time
printRel2 graphname rel = "digraph " ++ graphname ++ "{\n" 
                          ++ printLable (Set.elems (ent rel)) 
                          ++ printEdage (pairs rel) ++ "}"

printLable (s : ss) =  "\"" ++ show s ++ "\"" ++ " [ shape=box ] \n" ++ printLable ss
printLable [] = []  
printEdage (s : ss) = "\"" ++ show (fst s) ++ "\"" ++ " -> " ++ 
                      "\"" ++ show (snd s) ++ "\"" ++ "[ ] \n" ++ printEdage ss
printEdage []       = []


-- draw color of a sliced part 
--printSliceRel :: GraphName -> String -> Rel String String -> Rel String String -> String 
-- String is the node
-- first Rel is the whole rel
-- second Rel is the sliced rel
printSliceRel graphname node rel rel1 
                      = "digraph " ++ graphname ++ "{ \n" 
                        ++ printSliceLable node (Set.elems (ent rel)) (Set.elems (ent rel1)) 
                        ++ printSliceEdage (pairs rel) (pairs rel1)
                        ++ "}"

--printSliceLable :: String -> [String] -> [String] -> String  
-- first arg is the node string
-- second arg is the whole rel string 
-- third arg is the sliced sring
printSliceLable node (x : xs) aslice  
         | x == node = "\"" ++ show x ++ "\"" ++ " [ shape=box , style=filled , color=\"1 1 1\"] \n" ++ printSliceLable node xs aslice
         | x `elem` aslice = "\"" ++ show x ++ "\"" ++ " [ shape=box , style=filled , color=\".7 .3 1.0\"] \n" ++ printSliceLable node xs aslice
         | otherwise  = "\"" ++ show x ++ "\"" ++ " [ shape=box ] \n" ++ printSliceLable node xs aslice
printSliceLable node [] aslice = []

--printSliceEdage :: [(String , String)] -> [(String , String)] -> String
-- first arg is the whole edages 
-- second arg is the sliced fordward edages
-- third arg is the sliced backward edages
printSliceEdage  (x : xs) pair 
      | x `elem` pair = "\"" ++ show(fst x) ++ "\"" ++ " -> " ++ "\"" ++ show(snd x) ++ 
                        "\"" ++ " [ color=\"1 1 1\" ] \n" ++ printSliceEdage xs pair 
      | otherwise = "\"" ++ show(fst x) ++ "\"" ++ " -> " ++ "\"" ++ show(snd x) ++ 
                     "\"" ++ " [ ] \n" ++ printSliceEdage xs pair 
printSliceEdage [] pair = []
   
   
-----------------------------------------------------------------------------
-- * Parsing spreadsheet-like input files.

readMatrix :: Char -> Int -> FilePath -> IO [[String]]
readMatrix sepChar n fileName
  = do content <- readFile fileName
       let ls = take n (lines content)
       return (map stripTokens ls)
    where  
      stripTokens
        = map (unwords . words) . tokenizeBy (sepChar==)

-- | Tokenize an input lines by a given separator character predicate.
tokenizeBy :: (Char -> Bool) -> String -> [String]
tokenizeBy p "" = []
tokenizeBy p s    
 = let (l,s') = break p s
   in l : case s' of []      -> []
                     (_:s'') -> tokenizeBy p s''

-----------------------------------------------------------------------------
-- * Convert to relations.

-- | Convert a matrix to a relation. Every cell gives rise to a pair with 
--   first element the key from the first column in the corresponding
--   row, and with second element a pair of the cell value and the column
--   number. Cells with null values do not give rise to any pair.
matrixToRel :: [[String]] -> Rel String (String, Int)
matrixToRel mx
  = foldr (\row rel -> rowToRel rel row) emptyRel mx

rowToRel :: Rel String (String, Int) 
         -> [String] 
         -> Rel String (String, Int)
rowToRel rel (h:t)
  = foldr (addPair h) rel (zip t [1..])
    where
      addPair x y@(cell,n) rel
        | isNullValue cell
        = rel
        | otherwise
        = Set.insert (x,y) rel
rowToRel rel _ = rel

isNullValue "0" = True
isNullValue ""  = True
isNullValue "." = True
isNullValue _   = False

-----------------------------------------------------------------------------
-- * Auxiliaries

type FilterMask = [Bool]

-- | Keep or discard element of a list, depending on the truth value at the
--   corresponding position in a given filter mask. If the mask is shorter
--   than the given list, the list is truncated.
filterWithMask :: FilterMask -> [x] -> [x]       
filterWithMask mask l
  = concatMap decide (zip mask l)
    where
      decide (True,x) = [x]
      decide (_,_)    = []

-- | Compute the sets of values that occur in each collumn of a given
--   matrix.
valueSets :: Ord a => [Set a] -> [[a]] -> [Set a]
valueSets sets [] = sets
valueSets sets (r:rows) = valueSets (insrt r sets) rows
  where
    insrt [] sets = sets
    insrt row [] = map Set.singleton row
    insrt (c:cells) (s:sets) = (Set.insert c s) : insrt cells sets

-----------------------------------------------------------------------------
-- Partial Order

class PartialOrd a where
  maybeCompare :: a -> a -> Maybe Ordering
  (.<=.) :: a -> a -> Bool
  x .<=. y = maybe False (/=GT) $ maybeCompare x y
  (.>.)  :: a -> a -> Bool
  x .>. y = maybe False (==GT) $ maybeCompare x y
  (.>=.) :: a -> a -> Bool
  x .>=. y = maybe False (/=LT) $ maybeCompare x y
  (.<.)  :: a -> a -> Bool
  x .<. y = maybe False (==LT) $ maybeCompare x y
  (.==.)  :: a -> a -> Bool
  x .==. y = maybe False (==EQ) $ maybeCompare x y
  (./=.)  :: a -> a -> Bool
  x ./=. y = maybe False (/=EQ) $ maybeCompare x y
  (.||.)  :: a -> a -> Bool
  x .||. y = maybe True (const False) $ maybeCompare x y

instance Ord a => PartialOrd (Set a) where
  maybeCompare x y = case (Set.null xMy, Set.null yMx) of
      (False,False) -> Nothing
      (True,False)  -> Just LT
      (False,True)  -> Just GT
      (True,True)   -> Just EQ
    where xMy = x `Set.difference` y
          yMx = y `Set.difference` x

--------------------------
-- * Point free relations
-- See 
--   Functional dependency theory made ?simpler?
--   J.N. Oliveira
--   Techn. Report DI-PURe-05.01.01
--   2005, January

-- | Test whether relation is reflexive.
isReflexive :: Ord a => Rel a a -> Bool
isReflexive r = id .<=. r
  where id = identityRel (ent r)

-- | Test whether relation is coreflexive.
isCoreflexive :: Ord a => Rel a a -> Bool
isCoreflexive r = r .<=. id
  where id = identityRel (ent r)

-- | Test whether relation is simple.
isSimple :: (Ord a, Ord b) => Rel a b -> Bool
isSimple r = isCoreflexive (img r)

-- | Test whether relation is entire.
isEntire :: (Ord a, Ord b) => Rel a b -> Bool
isEntire r = isReflexive (ker r)

-- | Test whether relation is injective.
isInjective :: (Ord a, Ord b) => Rel a b -> Bool
isInjective r = isCoreflexive (ker r)

-- | Test whether relation is surjective.
isSurjective :: (Ord a, Ord b) => Rel a b -> Bool
isSurjective r = isReflexive (img r)

-- | Test whether relation is a representation.
isRepresentation :: (Ord a, Ord b) => Rel a b -> Bool
isRepresentation r = isInjective r && isEntire r 

-- | Test whether relation is functional.
isFunction :: (Ord a, Ord b) => Rel a b -> Bool
isFunction r = isEntire r && isSimple r 

-- | Test whether relation is an abstraction.
isAbstraction :: (Ord a, Ord b) => Rel a b -> Bool
isAbstraction r = isSimple r && isSurjective r 

-- | Test whether relation is an injective function.
isInjection :: (Ord a, Ord b) => Rel a b -> Bool
isInjection r = isRepresentation r && isFunction r

-- | Test whether relation is a surjective function.
isSurjection :: (Ord a, Ord b) => Rel a b -> Bool
isSurjection r = isFunction r && isAbstraction r

-- | Test whether relation is an isomorphism.
isBijection :: (Ord a, Ord b) => Rel a b -> Bool
isBijection r = isInjection r && isSurjection r    

-----------------------------------------------------------------------------
-- * Closures

-- | Compute the reflective closure
reflClose       :: Ord a => Rel a a -> Rel a a
reflClose xs    = xs `Set.union` Set.map (\x -> (x,x)) (ent xs)

-- | Compute the symmetric closure
symmClose       :: Ord a => Rel a a -> Rel a a
symmClose xs    = xs `Set.union` Set.map (\(x,y) -> (y,x)) xs
            
-- | Compute the transitive closure (naive, slow)
transClose'       :: Ord a => Rel a a -> Rel a a
transClose' xs    = if xxs == xs then xs else (transClose' xxs)
                   where 
                     xxs = Set.union xs (comp xs xs)

-- | Compute the transitive closure (faster)
transClose       :: Ord a => Rel a a -> Rel a a
transClose xs
  = Set.fromList $ concatMap tcFrom [ x | (x,_) <- Set.elems xs ]
    where 
      tcFrom x = [ (x,y) | y <- Set.elems (reachableFrom x) ]
      reachableFrom x
        = rng (slice (Set.singleton x) xs)

-- | Compute the reflexive, transitive closure
reflTransClose    :: Ord a => Rel a a -> Rel a a
reflTransClose xs = reflClose (transClose xs)

-- | Compute the reflexive, transitive, symmetric closure, i.e. the
--   induced equivalence relation.
equiv      :: Ord b => Rel b b -> Rel b b
equiv r    = reflTransClose (r `Set.union` (inv r))

-----------------------------------------------------------------------------   
----
-- | Type of functional dependencies.
data FunDep a b c d = FunDep { 
  antecedent :: Rel b d, 
  consequent :: Rel a c
 } deriving Show

-- | Create a functional dependency from two functions
--   and a relation.
mkFunDep :: (Ord a, Ord b, Ord c, Ord d) 
         => (b -> d) -> (a -> c) -> Rel b a 
         -> FunDep a b c d 
mkFunDep g f rel = FunDep gRel fRel
  where fRel = mkRel [ (b,f b) | (_,b) <- pairs rel ]
        gRel = mkRel [ (a,g a) | (a,_) <- pairs rel ]

-- | Test satisfaction of a functional dependency by
--   a relation.
satisfiesFunDep :: (Ord a, Ord b, Ord c, Ord d) =>
                   FunDep a b c d -> Rel b a -> Bool
satisfiesFunDep fd r 
  = isSimple (g `comp` r `comp` inv f)
    where f = antecedent fd
          g = consequent fd

-- | Test satisfaction of a functional dependency by
--   a relation (alternative formulation).
satisfiesFunDep' :: (Ord a, Ord b, Ord c, Ord d) =>
                   FunDep a b c d -> Rel b a -> Bool
satisfiesFunDep' fd r 
  = r `comp` ker f `comp` inv r .<=. ker g
    where f = antecedent fd
          g = consequent fd

-- | Test satisfaction of a functional dependency by
--   a relation (another alternative formulation).
--satisfiesFunDep'' :: (Ord a, Ord b, Ord c, Ord d) =>
--                   FunDep a b c d -> Rel b a -> Bool
satisfiesFunDep'' fd r 
  = ker (f `comp` inv r) .<=. ker g
    where f = antecedent fd
          g = consequent fd

-- | Test satisfaction of a functional dependency by
--   a relation (yet another alternative formulation).
satisfiesFunDep''' :: (Ord a, Ord b, Ord c, Ord d) =>
                   FunDep a b c d -> Rel b a -> Bool
satisfiesFunDep''' fd r 
  = isSimple $ projection g f r
    where f = antecedent fd
          g = consequent fd


-- | Test whether a given projection is a super key for
--   a given relation.
isSuperKey :: (Ord a, Ord b, Ord d) => Rel b d -> Rel b a -> Bool
isSuperKey x r
  = satisfiesFunDep (FunDep x id) r
    where id = identityRel (rng r)


-- | f,g-Projection of relation R

projection :: (Ord a, Ord b, Ord c, Ord d) =>
                   Rel a c-> Rel b d->Rel b a ->Rel d c
projection g f r 
  = (g `comp` r `comp` inv f)

-- | Standard Projection

proj :: (Ord a, Ord b) => Rel a b -> Rel a a -> Rel b b 
proj x = projection x x



-----------------------------------------------------------------------------
-- * Structure metrics

--------------------------------------
-- ** Tree impurity

-- | Tree impurity (TIMP): 
--   how much does the graph resemble a tree, expressed as
--   a percentage. A tree has tree impurity of 0 percent. A fully connected
--   graph has tree impurity of 100 percent. Fenton and Pfleeger only
--   defined tree impurity for non-directed graphs without self-edges. This
--   implementation therefore does not count self-edges, and counts 2 directed
--   edges between the same nodes only once.
treeImpurity :: Ord a => Gph a -> Float
treeImpurity g 
  | n==2 || n==1 || n==0
  -- In these cases, the max number of edges more than the spanning tree
  -- is 0, so the tree impurity should also be 0.
  = 0
  | otherwise
  = (2 * (e - n + 1)) / ((n - 1) * (n - 2)) * 100
  where e = (realToFrac $ Set.size $ reflReduc $ symmClose g) / 2
        n = realToFrac $ Set.size $ ent g

-- | Tree impurity after transitive closure:
-- This metric differs computes tree impurity not on the graph
--   itself, but on its transitive closure.
treeImpurityTC :: Ord a => Gph a -> Float
treeImpurityTC g = treeImpurity $ transClose g

--------------------------------------
-- ** Component metrics

-- | A record type to hold metrics about strong components.
data StrongComponentMetrics = StrongComponentMetrics {
   -- | Count of strong components
   componentCount :: Int,
   -- | Normalized count of strong components
   componentCountNormalized :: Float,
   -- | Count of non-singleton components
   nonSingletonComponentCount :: Int,
   -- | Size of largest component
   componentSizeMax :: Int,
   -- | Height of component DAG
   heightOfComponentGraph :: Int
 } deriving (Show,Eq)

-- | Calculate the strong component metrics for a given graph.
calculateStrongComponentMetrics :: Ord a => Gph a -> StrongComponentMetrics
calculateStrongComponentMetrics g
  = calculateStrongComponentMetrics' (ent g) (strongComponentGraph g)

-- | Calculate the strong component metrics for a given component graph.
calculateStrongComponentMetrics' 
  :: Ord a 
  => Set a   -- ^ Nodes of the underlying graph, need for normalized component count.
  -> Graph (Set a) 
  -> StrongComponentMetrics
calculateStrongComponentMetrics' nodes sCG 
  = calculateComponentMetrics heightDag nodes sCG

-- | Calculate the strong component metrics for a given component graph.
calculateComponentMetrics 
  :: Ord a 
  => (Gph (Set a) -> Int)   -- ^ Appropriate height function
  -> Set a   -- ^ Nodes of the underlying graph, needed for normalized component count.
  -> Graph (Set a) 
  -> StrongComponentMetrics
calculateComponentMetrics height nodes sCG = StrongComponentMetrics {
   componentCount = l,
   componentCountNormalized = l `asPercentageOf` n,
   nonSingletonComponentCount = Set.size sNSCS,
   componentSizeMax = maximum $ (0:) $ map Set.size $ Set.elems sCS,
   heightOfComponentGraph = height sCR
 } where
    sCS = fst sCG
    sCR = snd sCG
    sNSCS = Set.filter (\c -> Set.size c > 1) sCS
    l = Set.size sCS
    n = Set.size $ nodes

-- | Count of levels (LEV):
countOfLevels :: Ord a => Gph a -> Int
countOfLevels 
  = componentCount . calculateStrongComponentMetrics

-- | Normalized count of levels (CLEV):
normalizedCountOfLevels :: Ord a => Gph a -> Float
normalizedCountOfLevels 
  = componentCountNormalized . calculateStrongComponentMetrics

-- | Number of non-singleton levels (NSLEV)
numberOfNonSingletonLevels :: Ord a => Gph a -> Int
numberOfNonSingletonLevels
  = nonSingletonComponentCount . calculateStrongComponentMetrics

-- | Size of largest level (DEP):
sizeOfLargestLevel :: Ord a => Gph a -> Int
sizeOfLargestLevel
  = componentSizeMax . calculateStrongComponentMetrics
  
-- | Height for acyclic graphs (does not work correctly in the presence of cycles).
heightDag :: Ord a => Gph a -> Int
heightDag g = heightFrom g Set.empty (topNodes g)

-- | Compute graph height (length of longest path in terms of number of nodes),
--   starting from a given set of nodes.
heightFrom :: Ord a 
           => Gph a        -- ^ Graph
           -> Set a        -- ^ Already visited nodes
           -> Set a        -- ^ Nodes still to visit
           -> Int          -- ^ Number of nodes on the longest path
heightFrom g visited tovisit 
  = maximum $ (0:) $ map (+1) $ Set.elems heights
  where
    currentNodes = tovisit `Set.difference` visited
    heights = Set.map heightFrom1 currentNodes
    heightFrom1 n = heightFrom g (Set.insert n visited) (rngWith (Set.singleton n) g)

-- | Compute the longest path through a graph.
height       :: Ord a => Gph a -> Int
height g
  = Set.fold (\(x,_) n -> n `max` longestFrom x) 0 g
    where 
      longestFrom x 
        = heightUntil (Set.singleton x) Set.empty (outEdgesMap g)

-- | Compute longest path starting from seeds, stopping at stoppers.
heightUntil 
  :: Ord a 
  => Set a        -- ^ Nodes still to visit (seeds) 
  -> Set a        -- ^ Already visited nodes (stoppers) 
  -> Map a [a]    -- ^ Map of nodes to their immediate kids
  -> Int          -- ^ Number of nodes on the longest path
heightUntil seed stop childMap
  = if (Set.null s)
      then 0 
      else 1 + (heightUntil seed' stop' childMap)
    where
      s     = seed Set.\\ stop
      cm'   = Map.filterWithKey (\x _ -> x `Set.member` s) childMap 
      seed' = Map.foldr' (\ys s' -> s' `Set.union` (Set.fromList ys)) Set.empty cm' 
      stop' = stop `Set.union` s      

-- | Helper function that transforms a graph into a map
--   from nodes to their child lists. Used for optimization.
outEdgesMap :: Ord a => Gph a -> Map a [a]
outEdgesMap g
  = Set.fold worker Map.empty g
    where
      worker (x,y) mp 
        = Map.insertWith (\_ ys -> (y:ys)) x [y] mp
        
-- | Height (works also in the presence of cycles, but very slow).
heightSlow :: Ord a => Gph a -> Int
heightSlow g 
  = (heightFrom newG Set.empty (Set.singleton Nothing)) - 1
  where
    justG = Set.map (\(x,y)->(Just x, Just y)) g
    newEdges = Set.map (\x -> (Nothing, x)) (dom justG)
    newG = newEdges `Set.union` justG 

-----------------------------------------------------------------------------
-- ** Fan in and out

-- | Compute fan-in and fan-out of a given graph. Both are represented
--   with a bags of nodes. The arity of each node in the bag is its
--   fanout or fanin, repectively.
fanInOut :: Ord a => Gph a -> (Bag a, Bag a)
fanInOut g = foldr fan1 (emptyBag,emptyBag) $ Set.elems g
  where
    fan1 (a,b) (fanIn,fanOut)
      = (addToBag fanIn b, addToBag fanOut a)
      
-----------------------------------------------------------------------------
-- ** Coupling and coherence

-- | Given a partition of the domain into components (sets of nodes),
--   compute for each component the number of outgoing edges, incoming
--   edges, internal edges, and instability. 
couplings 
  :: (Ord a, Integral n)
  => Set (Set a)                       -- ^ groups
  -> Gph a                             -- ^ graph
  -> Map (Set a) (n,n,n,Float,Float)   -- ^ map from groups to coupling metrics
couplings aas g = Set.fold worker Map.empty aas
  where
    worker as mp = Map.insert as (coupling as g) mp

coupling :: (Ord a, Integral n)
          => Set a -> Gph a -> (n,n,n,Float,Float)
coupling as g = (ce,ca,ci,instability,coherence)
  where
    (ce,ca,ci) = Set.fold worker (0,0,0) g
    instability = ce `asPercentageOf` (ca+ce)
    coherence   = if Set.size as /= 1
                     then ci `asPercentageOf` (ca+ce+ci)
                     else 100   -- Singleton groups have 100% coherence.
    worker (x,y) (ce,ca,ci) 
      = case (x `Set.member` as,y `Set.member` as) of
          (True,True) -> (ce,ca,ci+1)
          (True,_)    -> (ce+1,ca,ci)
          (_,True)    -> (ce,ca+1,ci)
          otherwise   -> (ce,ca,ci)

-- | Helper for math.
asPercentageOf :: Real n => n -> n -> Float
asPercentageOf _ 0 = 0
asPercentageOf l n = (realToFrac l / realToFrac n) * 100
          
-----------------------------------------------------------------------------       


-----------------------------------------------------------------------------
------------------------------
-- * Formal Concept Analysis

type Context g m = Rel g m
type Concept g m = (Set g, Set m)
type ConceptLattice g m = Rel (Concept g m) (Concept g m)

extent :: Concept g m -> Set g
extent (gs,ms) = gs

intent :: Concept g m -> Set m
intent (gs,ms) = ms

-----------------------------------------------------------------------------
-- * Basic operations

{-
-- | The neighborhood of a set of nodes in a graph are the nodes to which 
--   they are connected direclty, excluding the nodes themselves.
neighborhood :: Ord a => Set a -> Rel a a -> Set a
neighborhood seeds r
  = rngWith seeds r `Set.difference` seeds

-- | The universal nodes in a graph are the nodes that are directly 
--   connected to all other nodes in the graph.
universals :: Ord a => Rel a a -> Set a
universals r = Set.fromList [ a | a <- Set.toList entr, 
                           entr == flip Set.insert (seenFrom a) a ]
  where
    entr = ent r
    seenFrom a
      = Set.fromList [ y | (x,y) <- Set.toList r, x==a ]
        `Set.union`
        Set.fromList [ x | (x,y) <- Set.toList r, y==a ]
-}

-- | Compute the extent of a concept from a given intent, given a
--   formal context.
extentFromIntent :: (Ord g, Ord m) => Context g m -> Set m -> Set g
extentFromIntent cxt intent
  = Set.fromList [ g | g <- Set.toList (dom cxt),  
                and [ (g,m) `Set.member` cxt | m <- Set.toList intent ]
          ]

-- | Compute the intent of a concept from a given extent, given a
--   formal context.
intentFromExtent :: (Ord g, Ord m) => Context g m -> Set g -> Set m
intentFromExtent cxt extent
  = Set.fromList [ m | m <- Set.toList (rng cxt),  
                and [ (g,m) `Set.member` cxt | g <- Set.toList extent ]
          ]

conceptsFromIntent :: (Ord g, Ord m) => Context g m -> Set m -> [Concept g m]
conceptsFromIntent cxt intent
  = undefined

-- | From the extent of a given context, compute all concepts greater than
--   that context.
conceptsFromExtent :: (Ord g, Ord m) => Context g m -> Set g -> [Concept g m]
conceptsFromExtent cxt extent
  = [ let g' = extent `Set.union` (Set.singleton g)
          i  = intentFromExtent cxt g'
          e  = extentFromIntent cxt i
      in (e,i)
      | g <- Set.toList (dom cxt), not (g `Set.member` extent) ]

-- | Produce the dual of a given lattice.
dualConceptLattice :: (Ord g, Ord m) 
                   => ConceptLattice g m -> ConceptLattice m g
dualConceptLattice l
  = Set.map dual l
    where
      dual ((g1,m1),(g2,m2)) = ((m2,g2),(m1,g1))

-----------------------------------------------------------------------------
-- * Predicates

isConceptOf :: (Ord g, Ord m) => Concept g m -> Context g m -> Bool
isConceptOf (g,m) cxt
  = g `isSubsetOf` dom cxt &&
    m `isSubsetOf` rng cxt &&
    extentFromIntent cxt m == g &&
    intentFromExtent cxt g == m 
    where
      isSubsetOf sub super = Set.null (sub `Set.difference` super)

conceptGT :: (Ord g, Ord m) => Concept g m -> Concept g m -> Bool
conceptGT (g1,m1) (g2,m2)
  = g2 `isStrictSubsetOf` g1 &&
    m1 `isStrictSubsetOf` m2
    where
      isStrictSubsetOf sub super 
        = Set.null (sub `Set.difference` super) &&
          not (Set.null (super `Set.difference` sub))
      
-----------------------------------------------------------------------------
-- * Fast Concept Analysis by Christian Lindig

-- | Compute the upper neighbors of a concept.
neighbors :: (Ord g, Ord m) 
          => Set g             -- ^ extent of concept 
          -> Context g m       -- ^ formal context
          -> [Concept g m]     -- ^ list of neighbors
neighbors extent cxt
  = worker (Set.toList extentComp) extentComp []
    where
      extentComp = dom cxt `Set.difference` extent 
      worker [] min ns = ns
      worker (g:gs) min ns
        = worker gs min' ns'
          where
            m1 = intentFromExtent cxt (flip Set.insert extent g)
            g1 = extentFromIntent cxt m1
            new = g `Set.delete` (g1 `Set.difference` extent) 
            (min',ns')
               = if (Set.null (min `Set.intersection` new))
                  then (min, (g1,m1):ns)
                  else (flip Set.delete min g,ns)

-- | Computes the concept lattice of a given formal context.
lattice :: (Ord g, Ord m) 
        => Context g m         -- ^ formal context
        -> ConceptLattice g m  -- ^ corresponding concept lattice
lattice cxt = worker emptyRel [] [(initExtent,initIntent)]
  where
    initIntent = intentFromExtent cxt Set.empty
    initExtent = extentFromIntent cxt initIntent
    worker l done [] = l
    worker l done (c@(extent,intent):todo)
      | extent `elem` done
      = worker l done todo
      | otherwise
      = worker l' done' todo'
        where
          ns = neighbors extent cxt
          l' = Set.fromList (map (\c' -> (c',c)) ns) `Set.union` l
          done' = (extent:done)
          todo' = ns ++ todo
{-
condenseLattice :: (Ord g, Ord m) 
        => ConceptLattice g m -> ConceptLattice g m
condenseLattice l
  = Set.map (\(x,y) -> (condenseConcept x, condenseConcept y)) l
    where
      condenseConcept c@(g,m) 
        = ( g `Set.differences` Set.map fst (domWith (Set.singleton c) l)
          , m `Set.differences` Set.map snd (domWith (Set.singleton c) l)
          )

Set.differences :: Ord g => Set g -> Set (Set g) -> Set g
--Set.differences g ss =  (foldr (\s g -> g `minusSet` s) g (Set.toList ss))
Set.differences g ss = intersectManySets (Set.toList ss)

intersectManySets [] = Set.empty
intersectManySets ss = foldr1 Set.intersection ss 
-}

-----------------------------------------------------------------------------
-- * Printing concept lattices.

-- | Compute the difference between a concept and its immediate neighbors.
--   Thus, the concept's extent is reduced to the objects that are not 
--   present among its lower neighbors, and and its intent is reduced to
--   the attributes that are not present among its upper neighbors.
--   Useful when displaying a concept lattice in condensed form.
conceptDelta :: (Ord g, Ord m)
             => ConceptLattice g m 
             -> Concept g m
             -> Concept g m
conceptDelta l c@(g,m)
  = (g',m')
    where 
      g' = g `Set.difference` Set.unions lowerExtents
      m' = m `Set.difference` Set.unions upperIntents
      lowerExtents = [ g'' | (c',(g'',_)) <- Set.toList l, c'==c ]
      upperIntents = [ m'' | ((_,m''),c') <- Set.toList l, c'==c ]

-- | Print a concept lattice as a dot graph.
printConceptLattice :: (Ord g, Ord m)
                    => (Set g -> String)
                    -> (Set m  -> String)
                    -> ConceptLattice g m
                    -> String
printConceptLattice showG showM l
  = printConceptLatticeWith showG showM showConcept l
    where
      showConcept (g,m)
        = (showG g)++"\\n"++(showM m)

-- | Print a concept lattice as a dot graph. Extra parameters for
--   controlling the labels for concept nodes.
printConceptLatticeWith :: (Ord g, Ord m)
                    => (Set g -> String)
                    -> (Set m  -> String)
                    -> (Concept g m -> String)
                    -> ConceptLattice g m
                    -> String
printConceptLatticeWith showG showM showConcept l
  = printGraph "lattice" l
    where
      printGraph 
        = printGraphWith conceptToNodeName showConceptLabel
      conceptToNodeName (g,m)
        = (showG g)++"\\n"++(showM m)
      showConceptLabel l c
        = showConcept (conceptDelta l c)

sepWith :: a -> [a] -> [a]
sepWith _ [] = []
sepWith a [x] = [x]
sepWith a (x:xs) = x : a : sepWith a xs

-----------------------------------------------------------------------------

-- a formal context for testing purposes.
cxt1
  = Set.fromList [('a','2'),('a','3'),('a','6'),
           ('b','1'),('b','2'),('b','3'),
           ('c','1'),('c','2'),('c','5'),
           ('d','1'),('d','4'),('d','5'),
           ('e','1'),('e','4'),
           ('f','3')]

eg1 = Set.fromList [(1,2),(2,3),(1,6),(5,4),(3,6),(7,9),(2,6),(6,1),(100,101),(102,100),(101,102)]
-----------------------------------------------------------------------------    

--- zyz
--
imis2r :: IntMap IntSet -> Rel Int Int
imis2r idm = Set.unions [mkRelNeighbors a (IS.toList bs)
                      | (a,bs) <- IM.toList idm]

set2r :: (Ord b, Ord a) => Set (a,b) -> Rel a b
set2r = id

g2r :: G.Graph gr => gr a b -> Rel Int Int
g2r g = mkRel (G.edges g)

r2g :: Rel Int Int -> G.Gr () ()
r2g r = G.mkUGraph (Set.toList $ ent r) (pairs r)

map2r :: (Ord b, Ord a) => Map a b -> Rel a b
map2r = mkRel . Map.toList

sliceB vs r = sliceBackward (Set.fromList vs) r
sliceF vs r = slice (Set.fromList vs) r
chop2 vs1 vs2 r = chop (Set.fromList vs1) (Set.fromList vs2) r

--
printRelSli vars r r2 = printGr "Slice.dot" $ printRelSlice vars r r2

printInstRel fn = printGr fn. printGraphWith (show) (const $ quote . show)
                    "LDG_inst"
printSrcRel fn = printGr fn. printGraphWith (show) (const $ quote . show)
                    "LDG_srcln"                    
printRelGr r1 r2 = printGr "LDGraph.dot" $ printRelTwo "LDG2" r1 r2

showMap :: (Show a, Show b) => Map a b -> String
showMap = Map.showTreeWith (\k x -> show (k,x)) True False

showMap' :: (Show a, Show b) => Map (Set a) b -> String
showMap' = Map.showTreeWith showKV False False
   where  showKV k x = "(" ++ show (Set.toList k) ++ ", " ++ show x ++ ")"

----  
printRelMetrics :: (Ord a, Show a) => Gph a -> IO ()
printRelMetrics r = putStrLn metricInfo  
  where 
    entSet = ent r
    reachRel = transClose r      -- reach $ lrel True r
    scc = strongComponentSet r
    scc' = strongNonSingletonComponentSet r  
    sccRel = snd $ strongComponentGraph r 
    coupCohMap = couplings scc' r
    coupleMap = Map.map (\(a,b,c,d,e)->(a,b,d)) coupCohMap 
    coherenceMap = Map.map (\(a,b,c,d,e)->(a,b,c,e)) coupCohMap 
    --          
    relSize = "\n\tGraph(#Nodes,#Edges) = " ++ "(" ++ (show $ Set.size entSet) 
               ++ ", " ++ (show $ Set.size r) ++ ")"
    treeImpure = "\n\tTree_Impurity = " ++ (show $ treeImpurity r)
    
    sccInfo = "\n\tSCC = " ++ (show . Set.toList $ Set.map Set.toList scc)
    compMetric = "\n\tSCC_Metrics = " ++ (show $ calculateStrongComponentMetrics r)
    couples = "\n\tCouplings(#outEdg,#inEdg,instability) = \n" ++ (showMap' coupleMap)    
    coherences = "\n\tCoherences(#outEdg,#inEdg,#interEdg,coherence) = \n" ++ (showMap' coherenceMap)
    --    
    metricInfo = concat ["\nIts some metrics Info.:", 
            relSize,treeImpure,sccInfo,compMetric,couples,coherences,"\n\n"]                        