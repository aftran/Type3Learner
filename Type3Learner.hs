module Type3Learner where

import Data.Set as S
import Data.Map as M
import Data.Maybe
import GraphA
import Data.Monoid
import List

type Monomial f = Set f

monomialFromList :: (Ord f) => [f] -> Monomial f
monomialFromList = S.fromList

-- A morph with an integer subscript.
data Mi w = Mi w Int
    deriving (Eq, Ord, Show)

type Lexeme f = Map Int (Monomial f)

type Lexicon w f = Map w (Lexeme f)

type Table w f = Map (Monomial f) (Set (Mi w))

data Hypothesis w f = Hypothesis (Lexicon w f) (GraphA (Mi w))

-- In terms of Type3learner, matches e t = the morphs predicted to appear in
-- environment e, according to table t.
-- matches e t = the union of all values of t whose key is a subset of e.
matches :: (Ord f, Ord w) => Monomial f -> Table w f -> Set (Mi w)
matches e = S.unions . M.elems . M.filterWithKey f
  where f k _ = e `S.isSubsetOf` k

-- seenMorphs e t = the Set associated with Monomial e in Table t, if it exists,
-- otherwise the empty Set.
-- In terms of Type3learner, seenMorphs e t = the (Mi w)s seen in environment
-- (Monomial) e, according to Table t.
seenMorphs :: (Ord f) => Monomial f -> Table w f -> Set (Mi w)
seenMorphs = M.findWithDefault S.empty

-- addToLexicon lexicon morph index monomial = the lexicon with the added
-- monomial (meaning) associated with the index of the morph.
addToLexicon :: (Ord w) => Lexicon w f -> w -> Int -> Monomial f -> Lexicon w f
addToLexicon lex morph idx mon = M.alter f morph lex
  where f = Just . fromMaybe (M.singleton idx mon) . fmap (M.insert idx mon)

emptyLexicon :: Lexicon w f
emptyLexicon = M.empty

emptyTable :: Table w f
emptyTable = M.empty

-- lexicalMeanings m l = the Lexeme associated with m in Lexicon f, if it
-- exists, otherwise the empty lexeme.
lexicalMeanings :: (Ord w) => w -> Lexicon w f -> Lexeme f
lexicalMeanings = M.findWithDefault M.empty

-- addToTable tbl monomial morph index = the table with (Mi morph index) added
-- to the set associated with monomial.
addToTable :: (Ord f, Ord w) => Table w f -> Monomial f -> w -> Int -> Table w f
addToTable tbl monomial morph idx = M.alter f monomial tbl
  where m = Mi morph idx
        f = Just . fromMaybe (S.singleton m) . (fmap $ S.insert m)

-- similarity s t = the cardinality of s intersected with t.
similarity :: (Ord f) => Set f -> Set f -> Int
similarity s t = S.size $ S.intersection s t

-- Produce a compare function for sorting Monomals by DISsimilatiry to the
-- given Monomial.
compareDissimTo :: (Ord f) => Monomial f -> Monomial f -> Monomial f -> Ordering
compareDissimTo e m1 m2 = compare (similarity e m2) (similarity e m1)

data State w f = State { lexicon       :: Lexicon w f
                       , blocking      :: GraphA (Mi w)
                       , freeVariation :: GraphA (Mi w)
                       , seen          :: Table w f
                       , predicted     :: Table w f } deriving Show

-- The State that the type-3 learner should be in before it has seen any text
-- elements.
emptyState :: (Ord w) => State w f
emptyState = State emptyLexicon GraphA.empty GraphA.empty M.empty M.empty

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

-- a `times` b = Data.Set.toList (the cartesian product of a and b).
times :: Set a -> Set b -> [(a,b)]
a `times` b = [(d,e) | d <- S.toList a, e <- S.toList b]

-- self f = a function that applies an endomorphism f to two of its argument.
-- If self is already implemented in an existing library, I couldn't find it on
-- Hoogle...
self :: (a -> a -> b) -> a -> b
self f x = f x x

-- update_blocking_row seen predicted br = br with a new edge added for each
-- pair in the cartesian product seen*(predicted-seen).  This means creating a
-- new blocking rule whenever a morph is predicted (but not seen) in the
-- environment where another morph has been seen. *)
updateBlockingRow ::
    (Ord w) => Set (Mi w) -> Set (Mi w) -> GraphA (Mi w) -> GraphA (Mi w)
updateBlockingRow s p br = foldr f br pairs
  where pairs     = s `times` (p `S.difference` s)
        f (x,y)   = addEdge x y

-- With ms = [(4,m1); (9,m2); (1,m3); ...] (for example),
-- intersect e ms total = (a,b), where:
--      a = e intersected with the monomial in the head of ms (if ms has a
--      head) or just e.
--      b = the integer in the head of ms (if ms has a head) or just total+1.
intersect :: (Ord f) =>
    Monomial f -> [(Int, Monomial f)] -> Int -> (Int, Monomial f)
intersect e ((i,mon):_) _     = (i,S.intersection mon e)
intersect e []          total = (total+1,e)
-- TODO: We don't necessarily need to see total.  Instead, use Either to
-- communicate to the caller whether we are intersecting with an element of the
-- lst or creating a new homophone.  (Or do something else that fixes the weird
-- allocation of responsibilities.)
-- However, people who read the paper about this algorithm will probably be
-- able to understand this code better if I keep the intersect function as it
-- is.

-- updateFreeVariationRow seenSet graph = the graph with a new edge added for
-- each pair in the cartesian product seenSet*seenSet except for the pairs of
-- the form (x,x).
updateFreeVariationRow :: (Ord w) =>
    Set (Mi w) -> GraphA (Mi w) -> GraphA (Mi w)
updateFreeVariationRow =
                       -- It might be easier to read these comments in reverse
                       -- order, because function composition applies the last
                       -- function first:
      appEndo          -- Remove the Endo wrapper, yielding a function
                       -- of type (GraphA (Mi w) -> GraphA (Mi w)).
    . mconcat          -- Compose the list of Endos together into a single Endo.
    . convertToAdders  -- Turn each pair into a function that adds the pair to
                       -- a GraphA as an edge.  (And wrap the function in an
                       -- Endo, so that mconcat uses normal function
                       -- composition ((.)).)
    . noSelfPairs      -- Remove pairs of the form (x,x) from the list.
    . self times       -- Cartesian-product the set with itself (yields a
                       -- list of pairs).
        where noSelfPairs     = List.filter (\(a,b) -> a /= b)
              convertToAdders = List.map    (\(a,b) -> Endo $ addEdge a b)

-- synchronize mi meaning environment state = a new State after adding "Mi w
-- int -> meaning" to the lexicon in response to seeing w in the given
-- environment.
synchronize :: (Ord f, Ord w) =>
    w -> Int -> Monomial f -> Monomial f -> State w f -> State w f
synchronize w i meaning environment state = State { lexicon       = newL
                                                  , blocking      = newB
                                                  , freeVariation = newF
                                                  , seen          = newS
                                                  , predicted     = newP }
  where s  = seen          state
        p  = predicted     state
        fv = freeVariation state
        l  = lexicon       state
        newL = addToLexicon l w i         meaning
        newP = addToTable   p meaning     w i
        newS = addToTable   s environment w i
        newF = updateFreeVariationRow (seenMorphs environment newS) fv
        newB = computeBlocking s p

-- Return a new state in response to the given state and the witnessing of
-- the given morph in the given environment.
getHypothesis :: (Ord f, Ord w) => w -> Monomial f -> State w f -> State w f
getHypothesis morph env state = foldr f lastResort sortedList
  where sortedList     = let lexeme   = lexicalMeanings morph (lexicon state)
                             comp a b = compareDissimTo env (snd a) (snd b)
                         in sortBy comp $ M.toList lexeme
        lastResort     = synchronize morph (1+length sortedList) env env state
        f (idx,mean) b = let state2 = synchronize morph idx mean env state
                             in if overlap state2 then b else state2

-- Return whether an overlap has been detected in the state.  Overlap is
-- defined in Pertsova (2010).
overlap :: (Ord w) => State w f -> Bool
overlap s = let b = blocking      s
                v = freeVariation s
                in (hasCycle b) || (freeVariationOverlap b v)

-- Compute whether an overlap has been detected due to the blocking rules br
-- causing the wrong prediction in some environment.  Equivalently, compute
-- whether there is a blocking rule between two Mis that are also in free
-- variation.
freeVariationOverlap :: (Ord w) => GraphA (Mi w) -> GraphA (Mi w) -> Bool
freeVariationOverlap b v =
    let [bList, vList] = fmap (S.fromList . edges) [b, v]
        in not . S.null $ vList `S.intersection` bList
