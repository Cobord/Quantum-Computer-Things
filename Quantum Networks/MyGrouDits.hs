module MyGrouDits
( Groudit2by2
) where

import MyGroups

-- https://arxiv.org/pdf/1707.00966.pdf --

data ObjectSet = Slot1 | Slot2 deriving (Eq,Show,Read)

class (Eq a) => FiniteGroupoid a where
    groupoidmult :: a -> a -> Maybe a
    groupoidinv :: a -> a
    identityMorphisms :: ObjectSet -> a
    sourceObject :: a -> ObjectSet
    targetObject :: a -> ObjectSet

class (FiniteGroupoid a) => FiniteGroudit a where
    --Pair of balancers sigma and tau that give bijections G_i with Obj(G)
    sigma :: a -> ObjectSet
    tau :: a -> ObjectSet
    -- if x :: a is x=(g,i) then sigma x is sigma_i (g), these can be reasonably complicated bijections if d big enough
    sigmaInverse :: ObjectSet -> ObjectSet -> a
    tauInverse :: ObjectSet -> ObjectSet -> a
    -- Equations 7 and 8
    sigma1 :: a -> (ObjectSet,ObjectSet)
    sigma2 :: (ObjectSet,ObjectSet) -> a
    tau1 :: a -> (ObjectSet,ObjectSet)
    tau2 :: (ObjectSet,ObjectSet) -> a
    -- From these balancers contstruct biunitary
    biunitary :: a -> a

data Groudit2by2 = Groudit2by2 { giPart::CyclicGroup2
                                       , whichObject :: ObjectSet
                                      } deriving (Eq,Show)

instance FiniteGroupoid Groudit2by2 where
    groupoidinv a = Groudit2by2 { giPart = inv ( giPart a) , whichObject = whichObject a}
    identityMorphisms b = Groudit2by2 { giPart = identity , whichObject = b}
    sourceObject a = whichObject a
    targetObject a = whichObject a
    groupoidmult a b
                     | (sourceObject a /= targetObject b) = Nothing
                     | otherwise = Just Groudit2by2 { giPart = mult (giPart a) (giPart b) , whichObject = whichObject a}

-- instance FiniteGroudit Groudit2by2 where ...