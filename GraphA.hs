module GraphA where

import qualified Data.Graph.Inductive.Graph as GC
import qualified Data.Graph.Inductive.Tree  as G
import Data.Map as M
import Data.Maybe

data GraphA a = GraphA (G.Gr a ()) (Map a GC.Node)

-- The empty GraphA.
empty :: (Ord a) => GraphA a
empty = GraphA GC.empty M.empty

addEdge :: (Ord a) => a -> a -> GraphA a -> GraphA a
addEdge x y = f . (insNode y) . (insNode x)
  where f (GraphA gr m) = GraphA gr2 m
          where gr2 = GC.insEdge (v1,v2,()) gr
                  where v1 = m ! x
                        v2 = m ! y

insNode :: (Ord a) => a -> GraphA a -> GraphA a
insNode a orig@(GraphA gr m) = fromMaybe def (fmap (const orig) (M.lookup a m))
  where def = GraphA gr2 m2
          where newNode = head $ GC.newNodes 1 gr
                m2      = M.insert a newNode m
                gr2     = GC.insNode (newNode, a) gr



--    | M.lookup a m == Nothing = GraphA gr2 m2
--    | otherwise               = original
--      where m2   = M.insert a node m
--            node = head $ GC.newNodes 1 gr
--            gr2  = GC.insNode (node, a) gr




instance (Show a) => Show (GraphA a) where
    show (GraphA g n) = show g
