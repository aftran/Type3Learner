module Type3Learner where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

type Monomial f = S.Set f

monomialFromList :: (Ord f) => [f] -> Monomial f
monomialFromList = S.fromList

-- A morph with an integer subscript.
data Mi w = Mi w Int
    deriving (Eq, Ord)

instance (Show w) => Show (Mi w) where
    show (Mi x i) = show x ++ "_" ++ show i

type Lexeme f = M.Map Int (Monomial f)

type Lexicon w f = M.Map w (Lexeme f)

type Table w f = M.Map (Monomial f) (S.Set (Mi w))

-- In terms of Type3learner, matches e t = the morphs predicted to appear in
-- environment e, according to table t.
-- matches e t = the union of all values of t whose key is a subset of e.
matches :: (Ord f, Ord w) => Monomial f -> Table w f -> S.Set (Mi w)
matches e = S.unions . M.elems . M.filterWithKey p
  where p k _ = e `S.isSubsetOf` k

-- addToLexicon lexicon morph index monomial = the lexicon with the added
-- monomial (meaning) associated with the index of the morph.
addToLexicon :: (Ord w) => Lexicon w f -> w -> Int -> Monomial f -> Lexicon w f
addToLexicon lex morph index monomial = M.alter f morph lex
  where f = Just . fromMaybe (M.singleton index monomial)

emptyLexicon :: Lexicon w f
emptyLexicon = M.empty

-- addToTable tbl monomial morph index = the table with (Mi morph index) added
-- to the set associated with monomial.
addToTable :: (Ord f) => Table w f -> Monomial f -> w -> Int -> Table w f
addToTable tbl monomial morph index = M.alter f monomial tbl
  where f = Just . fromMaybe (S.singleton $ Mi morph index)
