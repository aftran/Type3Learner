module GraphA (
      GraphA(..)
    , GraphA.empty
    , addEdge
    , addSymmetricEdge
    , hasCycle
) where
-- TODO: This module might could be replaced by Data.Graph.EasyGrapher.

import qualified Data.Graph.Inductive.Graph as GC
import qualified Data.Graph.Inductive.Tree  as G
import Data.Map                             as M
import Data.Graph.Inductive.Basic (gsel, preorderF)
import Data.Maybe
import Data.Graph.Inductive.Query.DFS (reachable, dff)

data GraphA a = GraphA (G.Gr a ()) (Map a GC.Node)

-- The empty GraphA.
empty :: (Ord a) => GraphA a
empty = GraphA GC.empty M.empty

-- Add an edge connecting the two nodes with the given labels (and add a node
-- for each label if one is not already in the graph).
addEdge :: (Ord a) => a -> a -> GraphA a -> GraphA a
addEdge x y = f . (insNode y) . (insNode x)
  where f (GraphA gr m) = GraphA gr2 m
          where gr2 = GC.insEdge (v1,v2,()) gr
                  where v1 = m ! x
                        v2 = m ! y

-- addSymmetricEdge x y = (addEdge y x) . (addEdge x y).
addSymmetricEdge :: (Ord a) => a -> a -> GraphA a -> GraphA a
addSymmetricEdge x y = (addEdge y x) . (addEdge x y)

-- Add a node to the graph.
insNode :: (Ord a) => a -> GraphA a -> GraphA a
insNode a orig@(GraphA gr m) = fromMaybe def (fmap (const orig) (M.lookup a m))
  where def = GraphA gr2 m2
          where newNode = head $ GC.newNodes 1 gr
                m2      = M.insert a newNode m
                gr2     = GC.insNode (newNode, a) gr

-- True iff the GraphA has a cycle.
hasCycle :: GraphA a -> Bool
hasCycle (GraphA g _) = graphHasCycle g

-- True iff the graph (G.Gr) has a cycle.
graphHasCycle :: G.Gr a b -> Bool
graphHasCycle g = not . Prelude.null . gsel f $ g
  where f context = elem (GC.node' context) (reachableNotSelf context g)

-- The list of Nodes that are reachable from the given Context, not including
-- the Node in the Context itself.
reachableNotSelf :: GC.Graph gr => GC.Context a b -> gr a b -> [GC.Node]
reachableNotSelf c g = preorderF (dff (GC.suc' c) g)

instance (Show a) => Show (GraphA a) where
    show (GraphA g n) = show g
