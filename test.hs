import Type3Learner


data Feature = Bright | Dim
     deriving (Show, Eq, Ord)

l2 = addToLexicon emptyLexicon "hi" 1 (monomialFromList [Dim])
l3 = addToLexicon l2 "hi" 2 (monomialFromList [Bright])

lexicons = do
    print l2
    print l3

t  = addToTable emptyTable (monomialFromList [Dim]) "hi" 4
t2 = addToTable t  (monomialFromList [Bright]) "woof" 2
t3 = addToTable t2 (monomialFromList [Bright]) "eat" 1

tables = do
    print t
    print t2
    print t3

main = do
    lexicons
    tables
