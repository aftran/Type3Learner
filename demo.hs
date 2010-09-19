import Type3Learner
import PrettyPrinting

data Signed a = Plus a | Minus a
    deriving (Eq, Ord)

instance (Show a) => Show (Signed a) where
    show (Plus  x) = "+" ++ (show x)
    show (Minus x) = "-" ++ (show x)

data Feature = A | B
    deriving (Show, Eq, Ord)

m1 = monomialFromList [Plus  A,  Minus B]
m2 = monomialFromList [Plus  A,  Plus  B]
m3 = monomialFromList [Minus A,  Plus  B]
m4 = monomialFromList [Minus A,  Minus B]

text1 = [("y",m1), ("x",m2), ("y",m3), ("x",m4)]
text2 = [("y",m1), ("y",m2), ("y",m3), ("x",m4)]

derivation1 = type3derivation text1
derivation2 = type3derivation text2

main = do putStrLn "TEXT ONE"
          putStrLn "========"
          prettyPrint derivation1
          putStrLn "TEXT TWO"
          putStrLn "========"
          prettyPrint derivation2