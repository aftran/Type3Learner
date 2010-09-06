{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}
module PrettyPrinting where

import Type3Learner
import Text.PrettyPrint.HughesPJ
import Data.Set as S
import Data.Map as M
import GraphA
import Data.List

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

-- Generate a pretty-printed Doc of a set that contains of Pretty things.
prettyDocSet :: (Pretty a) => Set a -> Doc
prettyDocSet = braces
             . hsep
             . punctuate comma
             . fmap prettyDoc
             . S.toList

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
                  (\(mon, set) -> prettyDoc mon <> colon $$ nest 4 (prettyDocSet set))
              . M.toList

instance (Pretty a, Ord a) => Pretty (GraphA a) where
    prettyDoc gr = vcat . fmap h $ pairs
      where pairs = edges gr
            h (x,y)  = prettyDoc x $$ nest tabs (text "->" <+> prettyDoc y)
            -- Line up the ->s vertically by padding (just like how I line up
            -- ->s and =s in this Haskell code):
            tabs     = 1 + (maximumBy compare firstWordLengths)
              where firstWordLengths = fmap ff pairs
                      where ff (x, _) = length $ prettyShow x
