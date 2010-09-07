{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}
module PrettyPrinting (
      prettyDoc
    , prettyShow
    , prettyPrint
    , prettyDocFreeVariation
) where

import Type3Learner
import Text.PrettyPrint.HughesPJ
import Data.Set as S
import Data.Map as M
import GraphA
import Data.List
import Control.Applicative

class (Show a) => Pretty a where
    prettyDoc   :: a -> Doc
    prettyDoc   = text . prettyShow

    prettyShow  :: a -> String
    prettyShow  = show . prettyDoc

    prettyPrint :: a -> IO ()
    prettyPrint = print . prettyDoc


instance Show w => Pretty (Mi w) where
    prettyDoc (Mi w i) = text (show w) <> char '_' <> int i

-- Applies to Monomial f (=== Set f).
instance Show f => Pretty (Set f) where
    prettyDoc = braces
              . hsep
              . punctuate comma
              . fmap (text . show)
              . S.toList

-- Generate a spread-out, brace-less Doc of a set that contains of Pretty
-- things.
prettyDocSet :: (Pretty a) => Set a -> Doc
prettyDocSet = vcat
             . fmap prettyDoc
             . S.toList
-- TODO: Stop repeating the above Pretty instance.

instance Show f => Pretty (Lexeme f) where
    prettyDoc = vcat
              . fmap
                  (\(i, mon) -> char '_' <> int i <+> equals <+> prettyDoc mon)
              . M.toList

instance (Show w, Show f) => Pretty (Lexicon w f) where
    prettyDoc = vcat
              . fmap
                  (\(morph, lexeme) -> text (show morph) <> prettyDoc lexeme)
              . M.toList

instance (Show w, Show f) => Pretty (Table w f) where
    prettyDoc = vcat
              . fmap
                  (\(mon, set) -> prettyDoc mon <> colon $$ nest tab (prettyDocSet set))
              . M.toList

instance (Pretty a, Ord a) => Pretty (GraphA a) where
    prettyDoc = prettifyGraph "->" (const True)

-- Pretty-print a graph (whose edges are filtered by eFilter), separating
-- adjacent edges with arrowMsg.  For example, if arrowMsg is " ~-> ", then each
-- edge between a and b will look like "a ~-> b".
prettifyGraph :: (Pretty a, Ord a) => String -> ((a,a) -> Bool) -> GraphA a -> Doc
prettifyGraph arrowMsg eFilter gr
             | Data.List.null pairs = char 'Ø'
             | otherwise            = vcat . fmap h $ pairs
  where pairs = Data.List.filter eFilter $ edges gr
        h (x,y) = prettyDoc x $$ nest tab (text arrowMsg <+> prettyDoc y)
          -- Line up the ->s vertically by adding extra space before it (just
          -- like how I line up ->s and =s in this Haskell code):
          where tab = 1 + (maximumBy compare firstWordLengths)
                  where firstWordLengths = fmap ff pairs
                          where ff (x, _) = length $ prettyShow x

-- Alternate pretty-printing method for graphs that are symmetric (not
-- directed).  Prints edges as "<->" and does not print "y <-> x" if "x <-> y"
-- is already being printed.  This is only appropriate for non-directed graphs
-- (graphs where x -> y iff y -> x).
prettyDocFreeVariation :: (Pretty a, Ord a) => GraphA a -> Doc
prettyDocFreeVariation = prettifyGraph "<->" (uncurry (<))

-- Literal strings that will be used in the pretty-printing of Hypothesis and
-- State.
lexMsg       = "Lexicon:"
brMsg        = "Blocking rules:"
fvMsg        = "Free variation relations:"
seenMsg      = "Seen:"
predictedMsg = "Reverse lexicon:"

-- How many spaces to prepend to certain lines (for grouping).
tab = 4
tabText :: (Pretty a) => a -> Doc
tabText = (nest tab) . prettyDoc

instance (Show w, Show f, Ord w) => Pretty (Hypothesis w f) where
    prettyDoc (Hypothesis l g) =    text lexMsg
                                 $$ tabText l
                                 $$ text brMsg
                                 $$ tabText g

instance (Show w, Show f, Ord w) => Pretty (State w f) where
    prettyDoc s =    prettyDoc (hypothesis s)
                  $$ text fvMsg
                  $$ tabText (freeVariation s)
                  $$ text seenMsg
                  $$ tabText (seen s)
                  $$ text predictedMsg
                  $$ tabText (predicted s)

instance (Show w, Show f, Ord w) => Pretty (Derivation w f) where
    prettyDoc (Derivation inputs states) = vcat . getZipList $
                                           announce . int <$> ZipList [1..]
                                           <*>
                                           (ZipList $ zipWith g inputs states)
      where announce number doc = text "Step" <+> number <> colon <+> doc
            g (morph,env) state =     text (show morph)
                                  <+> text "in"
                                  <+> text (show env)
                                  $$ prettyDoc state
                                  <> text "\n"
