import Type3Learner
import PrettyPrinting

data Signed a = Plus a | Minus a
    deriving (Show, Eq, Ord)

data Feature = A | B
    deriving (Show, Eq, Ord)

m1 = monomialFromList [Plus  A,  Minus B]
m2 = monomialFromList [Plus  A,  Plus  B]
m3 = monomialFromList [Minus A,  Plus  B]
m4 = monomialFromList [Minus A,  Minus B]

text1 = [("y",m1), ("x",m2), ("y",m3), ("x",m4)]
text2 = [("y",m1), ("y",m2), ("y",m3), ("x",m4)]

history1 = type3learn text1
history2 = type3learn text2

main = do print "TEXT ONE\n========"
          print history1
          print "\n\nTEXT TWO\n========"
          print history2
