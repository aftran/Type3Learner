import Type3Learner
import GraphA


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

g  = empty :: GraphA (Mi String)
g2 = addEdge (Mi "hi" 1) (Mi "woof" 2) g
-- Make sure it's harmless to re-add nodes -- it risks causing a Node Exception:
g3 = addEdge (Mi "hi" 1) (Mi "woof" 2) g2

graphs = do
    print g
    print g2

mySeen      = addToTable t3   (monomialFromList [Bright]) "BLOCKER" 1
myPredicted = addToTable mySeen (monomialFromList [Bright]) "BLOCKEE" 1
br = computeBlocking mySeen myPredicted

blockingRules = print br

main = do
    lexicons
    tables
    graphs
    blockingRules
