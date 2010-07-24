import Type3Learner


data Feature = Bright | Dim
     deriving (Show, Eq, Ord)

l2 = addToLexicon emptyLexicon "hi" 2 (monomialFromList [Dim])
l3 = addToLexicon l2 "hi" 1 (monomialFromList [Dim, Bright])

