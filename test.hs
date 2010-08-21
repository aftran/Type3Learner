import Type3Learner
import GraphA


data Feature = Bright | Dim
     deriving (Show, Eq, Ord)

dim    = monomialFromList [Dim]
bright = monomialFromList [Bright]

l2 = addToLexicon emptyLexicon "hi" 1 (dim)
l3 = addToLexicon l2 "hi" 2 (bright)

lexicons = do
    print l2
    print l3

t  = addToTable emptyTable (dim) "hi" 4
t2 = addToTable t  (bright) "woof" 2
t3 = addToTable t2 (bright) "eat" 1

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

mySeen      = addToTable t3   (bright) "BLOCKER" 1
myPredicted = addToTable mySeen (bright) "BLOCKEE" 1
br = computeBlocking mySeen myPredicted

blockingRules = print br

s :: State String Feature
s = State { lexicon       = emptyLexicon
          , blocking      = GraphA.empty
          , freeVariation = GraphA.empty
          , seen          = emptyTable
          , predicted     = emptyTable }

s2 = synchronize "woof" 3 bright bright s

states = do print s
            print s2

main = do lexicons
          tables
          graphs
          blockingRules
          states
