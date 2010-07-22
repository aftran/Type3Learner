module Type3Learner (
     Monomial,
     Lexicon,
     monomialFromList)
where

import Data.Set
import Data.Map

type Monomial f = Set f

-- A morph with an integer subscript.
data Mi w = Mi w Int

type Lexicon w f = Map (Mi w) [Monomial f]

type Miset w = Set (Mi w)

type Table w f = Map (Monomial f) (Miset w)

monomialFromList :: (Ord f) => [f] -> Monomial f
monomialFromList = Data.Set.fromList
