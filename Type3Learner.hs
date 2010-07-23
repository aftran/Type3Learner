module Type3Learner (
     Monomial,
     Lexicon,
     monomialFromList,
     matches)
where

import Data.Set
import Data.Map

type Monomial f = Set f

monomialFromList :: (Ord f) => [f] -> Monomial f
monomialFromList = Data.Set.fromList

-- A morph with an integer subscript.
data Mi w = Mi w Int
     deriving (Eq, Ord)

type Lexicon w f = Map (Mi w) [Monomial f]

type Table w f = Map (Monomial f) (Set (Mi w))

-- In terms of Type3learner, matches e t = the morphs predicted to appear in
-- environment e, according to table t.
-- matches e t = the union of all values of t whose key is a subset of e.
-- matches :: (Ord f, Ord w) => Monomial f -> Table w f -> Set (Mi w)
matches e = Data.Set.unions . Data.Map.elems . Data.Map.filterWithKey (\k _ -> e `isSubsetOf` k)
