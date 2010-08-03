module Type3Learner where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import GraphA

type Monomial f = S.Set f

monomialFromList :: (Ord f) => [f] -> Monomial f
monomialFromList = S.fromList

-- A morph with an integer subscript.
data Mi w = Mi w Int
    deriving (Eq, Ord, Show)

type Lexeme f = M.Map Int (Monomial f)

type Lexicon w f = M.Map w (Lexeme f)

type Table w f = M.Map (Monomial f) (S.Set (Mi w))

data Hypothesis w f = Hypothesis (Lexicon w f) (GraphA (Mi w))

-- In terms of Type3learner, matches e t = the morphs predicted to appear in
-- environment e, according to table t.
-- matches e t = the union of all values of t whose key is a subset of e.
matches :: (Ord f, Ord w) => Monomial f -> Table w f -> S.Set (Mi w)
matches e = S.unions . M.elems . M.filterWithKey f
  where f k _ = e `S.isSubsetOf` k

-- addToLexicon lexicon morph index monomial = the lexicon with the added
-- monomial (meaning) associated with the index of the morph.
addToLexicon :: (Ord w) => Lexicon w f -> w -> Int -> Monomial f -> Lexicon w f
addToLexicon lex morph idx mon = M.alter f morph lex
  where f = Just . fromMaybe (M.singleton idx mon) . fmap (M.insert idx mon)

emptyLexicon :: Lexicon w f
emptyLexicon = M.empty

emptyTable :: Table w f
emptyTable = M.empty

-- addToTable tbl monomial morph index = the table with (Mi morph index) added
-- to the set associated with monomial.
addToTable :: (Ord f, Ord w) => Table w f -> Monomial f -> w -> Int -> Table w f
addToTable tbl monomial morph idx = M.alter f monomial tbl
  where m = Mi morph idx
        f = Just . fromMaybe (S.singleton m) . (fmap $ S.insert m)

-- similarity s t = the cardinality of s intersected with t.
similarity :: (Ord f) => S.Set f -> S.Set f -> Int
similarity s t = S.size $ S.intersection s t

-- Produce a compare function for sorting Monomals by DISsimilatiry to the
-- given Monomial.
compareDissimTo :: (Ord f) => Monomial f -> Monomial f -> Monomial f -> Ordering
compareDissimTo e m1 m2 = compare (similarity e m2) (similarity e m1)

data State w f = State { lexicon   :: Lexicon w f
                       , blocking  :: GraphA (Mi w)
                       , seen      :: Table w f
                       , predicted :: Table w f } deriving Show
                       -- Leave the free-variation as a computed structure
                       -- unless there's an advantage to computing it each
                       -- time.  (The OCaml code seems to keep old free
                       -- variation pairs around despite recomputing them
                       -- all, which is at best useless...

-- hypothesis s = the hypothesis in state s.
hypothesis :: State w f -> Hypothesis w f
hypothesis s = Hypothesis (lexicon s) (blocking s)

-- Given a table of seen and predicted morphs, compute_blocking s p = the
-- blocking-rule digraph generated by this rule: Whenever a morph, a, is seen
-- where another, b, is predicted-but-not-seen, add a blocking rule from a to
-- b.
computeBlocking :: (Ord f, Ord w) => Table w f -> Table w f -> GraphA (Mi w)
computeBlocking seen predicted = M.foldWithKey f GraphA.empty seen
  where f k s br = let p = matches k predicted
                      in updateBlockingRow s p br

-- update_blocking_row seen predicted br = br with a new edge added for each
-- pair in the cartesian product seen*(predicted-seen).  This means creating a
-- new blocking rule whenever a morph is predicted (but not seen) in the
-- environment where another morph has been seen. *)
updateBlockingRow :: (Ord w) => S.Set (Mi w) -> S.Set (Mi w) -> GraphA (Mi w) -> GraphA (Mi w)
updateBlockingRow s p br = foldr f br pairs
  where pairs     = s `times` (p `S.difference` s)
        f (x,y)   = addEdge x y
        times a b = [(d,e) | d <- S.toList a, e <- S.toList b]

-- With ms = [(4,m1); (9,m2); (1,m3); ...] (for example),
-- intersect e ms total = (a,b), where:
--      a = e intersected with the monomial in the head of ms (if ms has a
--      head) or just e.
--      b = the integer in the head of ms (if ms has a head) or just total+1.
intersect :: (Ord f) => Monomial f -> [(Int, Monomial f)] -> Int -> (Int, Monomial f)
intersect e ((i,mon):_) _     = (i,S.intersection mon e)
intersect e []          total = (total+1,e)
-- TODO: We don't necessarily need to see total.  Instead, use Either to
-- communicate to the caller whether we are intersecting with an element of the
-- lst or creating a new homophone.  (Or do something else that fixes the weird
-- allocation of responsibilities.)

-- synchronize mi meaning environment state = a new State after adding "Mi w
-- int -> meaning" to the lexicon in response to seeing w in the given
-- environment.
synchronize :: (Ord f, Ord w) =>
    w -> Int -> Monomial f -> Monomial f -> State w f -> State w f
synchronize w i meaning environment state = State { lexicon   = newL
                                                  , blocking  = newB
                                                  , seen      = newS
                                                  , predicted = newP }
  where s = seen      state
        p = predicted state
        l = lexicon   state
        newL = addToLexicon l w i         meaning
        newP = addToTable   p meaning     w i
        newS = addToTable   s environment w i
        newB = computeBlocking s p
