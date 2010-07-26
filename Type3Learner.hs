module Type3Learner where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

type Monomial f = S.Set f

monomialFromList :: (Ord f) => [f] -> Monomial f
monomialFromList = S.fromList

-- A morph with an integer subscript.
data Mi w = Mi w Int
    deriving (Eq, Ord, Show)

type Lexeme f = M.Map Int (Monomial f)

type Lexicon w f = M.Map w (Lexeme f)

type Table w f = M.Map (Monomial f) [Mi w]

data Hypothesis w f br = Hypothesis (Lexicon w f) br

-- In terms of Type3learner, matches e t = the morphs predicted to appear in
-- environment e, according to table t.
-- matches e t = the union of all values of t whose key is a subset of e.
matches :: (Ord f, Ord w) => Monomial f -> Table w f -> [Mi w]
matches e = concat . M.elems . M.filterWithKey f
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
addToTable :: (Ord f) => Table w f -> Monomial f -> w -> Int -> Table w f
addToTable tbl monomial morph idx = M.alter f monomial tbl
  where m = Mi morph idx
        f = Just . fromMaybe [m] . (fmap $ (:) m)

-- similarity s t = the cardinality of s intersected with t.
similarity :: (Ord f) => (Monomial f) -> (Monomial f) -> Int
similarity s t = S.size $ S.intersection s t

data State w f br = State { lexicon    :: Lexicon w f
                          , blocking   :: br
                          , seen       :: Table w f
                          , predicted  :: Table w f }
                          -- Leave the free-variation as a computed structure
                          -- unless there's an advantage to computing it each
                          -- time.  (The OCaml code seems to keep old free
                          -- variation pairs around despite recomputing them
                          -- all, which is at best useless...

-- hypothesis s = the hypothesis in state s.
hypothesis :: State w f br -> Hypothesis w f br
hypothesis s = Hypothesis (lexicon s) (blocking s)
