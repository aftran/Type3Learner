module GraphA where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree    as G
import Data.Graph.Inductive.NodeMap as N

data GraphA a = GraphA (G.Gr a ()) (NodeMap a)

instance (Show a) => Show (GraphA a) where
    show (GraphA g n) = show g

-- The empty GraphA.
empty :: (Ord a) => GraphA a
empty = GraphA Data.Graph.Inductive.Graph.empty N.new

insEdge :: (Ord a) => a -> a -> GraphA a -> GraphA a
insEdge a1 a2 (GraphA g n) = let (newG, newN, _) = insMapNodes n [a1,a2] g
                                 newNewG         = insMapEdge newN (a1, a2, ()) newG
                                 in GraphA newNewG newN
