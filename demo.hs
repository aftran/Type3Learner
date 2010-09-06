import Type3Learner

data BinaryFeature a = Plus a | Minus a
    deriving (Show, Eq, Ord)

data FeatureName = A | B
    deriving (Show, Eq, Ord)

m1 = monomialFromList [Plus  A,  Minus B]
m2 = monomialFromList [Plus  A,  Plus  B]
m3 = monomialFromList [Minus A,  Plus  B]
m4 = monomialFromList [Minus A,  Minus B]

text1 = [("y",m1), ("x",m2), ("y",m3), ("x",m4)]
text2 = [("y",m1), ("y",m2), ("y",m3), ("x",m4)]

main = do print "TEXT ONE\n========"
          print $ type3learn text1
          print "\n\nTEXT TWO\n========"
          print $ type3learn text2
