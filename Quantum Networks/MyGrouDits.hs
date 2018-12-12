{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module MyGrouDits
( Groudit2by2
) where

import MyGroups

-- https://arxiv.org/pdf/1707.00966.pdf --

data ObjectSet = Slot1 | Slot2 deriving (Eq,Show,Read,Enum,Bounded)

-- Groupoid with 2 objects
-- more examples use this specific case so did first
class (Eq a) => FiniteGroupoid2 a where
    groupoidmult_2 :: a -> a -> Maybe a
    groupoidinv_2 :: a -> a
    identityMorphisms_2 :: ObjectSet -> a
    sourceObject_2 :: a -> ObjectSet
    targetObject_2 :: a -> ObjectSet

-- the general groupoid where the set of objects is b
-- the a itself stores the arrows
-- when passing (a,b) that is meant to be an arrow a
-- and that the source for that arrow is the second of that tuple
class (Enum b,Bounded b,Eq a) => FiniteGroupoid a b where
    groupoidmult :: (a,b) -> (a,b) -> Maybe (a,b)
    groupoidinv :: (a,b) -> (a,b)
    identityMorphisms :: b -> (a,b)
    sourceObject :: (a,b) -> b
    targetObject :: (a,b) -> b

unpack :: (Maybe a,b) -> Maybe (a,b)
unpack (Nothing,y) = Nothing
unpack (Just x,y) = Just (x,y)

-- a groupoid with 2 objects is a general groupoid
instance (FiniteGroupoid2 a) => (FiniteGroupoid a ObjectSet) where
    groupoidmult (x1,x2) (y1,y2) = unpack (groupoidmult_2 x1 y1,x2)
    groupoidinv (x1,x2) = (groupoidinv_2 x1,targetObject_2 x1)
    identityMorphisms x2 = (identityMorphisms_2 x2, x2)
    sourceObject (x1,x2) = sourceObject_2 x1
    targetObject (x1,x2) = targetObject_2 x1

-- a groupoid with 2 objects might be a grouBit if you provide extra data
class (FiniteGroupoid2 a) => FiniteGroudit2 a where
    --Pair of balancers sigma and tau that give bijections G_i with Obj(G)
    sigma_2 :: a -> ObjectSet
    tau_2 :: a -> ObjectSet
    -- if x :: a is x=(g,i) then sigma x is sigma_i (g), these can be reasonably complicated bijections if d big enough
    sigmaInverse_2 :: ObjectSet -> ObjectSet -> a
    tauInverse_2 :: ObjectSet -> ObjectSet -> a
    sigma1_2 :: a -> (ObjectSet,ObjectSet)
    sigma2_2 :: (ObjectSet,ObjectSet) -> a
    tau1_2 :: a -> (ObjectSet,ObjectSet)
    tau2_2 :: (ObjectSet,ObjectSet) -> a
    -- From these balancers contstruct biunitary
    biunitary_2 :: a -> a

-- a general groupoid might be a grouDit if you provide extra data
class (FiniteGroupoid a b) => FiniteGroudit a b where
    --Pair of balancers sigma and tau that give bijections G_i with Obj(G)
    sigma :: a -> b
    tau :: a -> b
    sigmaInverse :: b -> b -> a
    tauInverse :: b -> b -> a
    sigma1 :: a -> (b,b)
    sigma2 :: (b,b) -> a
    tau1 :: a -> (b,b)
    tau2 :: (b,b) -> a
    -- From these balancers contstruct biunitary
    biunitary :: (a,b) -> (a,b)

-- arrows are stored as the element in Z_2 and which object they are on
-- this is the groupoid that is made of two copies of BZ_2
-- whichObject says which copy talking about
data Groudit2by2 = Groudit2by2 { giPart::CyclicGroup2
                                       , whichObject :: ObjectSet
                                      } deriving (Eq)

instance Show Groudit2by2 where
    show x = (show $ giPart x) ++ " on object " ++ (show $ whichObject x)

-- this is a groupoid on 2 objects, show how
instance FiniteGroupoid2 Groudit2by2 where
    groupoidinv_2 a = Groudit2by2 { giPart = inv ( giPart a) , whichObject = whichObject a}
    identityMorphisms_2 b = Groudit2by2 { giPart = identity , whichObject = b}
    sourceObject_2 a = whichObject a
    targetObject_2 a = whichObject a
    groupoidmult_2 a b
                     | (sourceObject_2 a /= targetObject_2 b) = Nothing
                     | otherwise = Just Groudit2by2 { giPart = mult (giPart a) (giPart b) , whichObject = whichObject a}

-- list of a's, a function f to b's, a y that is desired and a default value
-- then select the first of that list such that f(x)=y
selectFirstMeetingCriteria :: (Eq b) => [a] -> (a -> b) -> b -> a -> a
selectFirstMeetingCriteria [] _ _ defaultVal = defaultVal
selectFirstMeetingCriteria (x:xs) f myB defaultVal = if (f x == myB) then x else (selectFirstMeetingCriteria xs f myB defaultVal)

mySwap :: (a,b) -> (b,a)
mySwap (x,y) = (y,x)

-- this is a grouBit, show how by specifying the extra data
instance FiniteGroudit2 Groudit2by2 where
    sigma_2 a
        | ((giPart a == Id) && (whichObject a == Slot1)) = Slot1 -- epsilon_1 (0) = 0
        | (giPart a == Flip) && (whichObject a == Slot1) = Slot2 -- epsilon_1 (1) = 1
        | (giPart a == Id) && (whichObject a == Slot2) = Slot1 -- epsilon_2 (0) = 0
        | (giPart a == Flip) && (whichObject a == Slot2) = Slot2 -- epsilon_2 (1) = 1
    tau_2 a
         | (giPart a == Id) && (whichObject a == Slot1) = Slot1 -- tau_1 (0)=0
         | (giPart a == Flip) && (whichObject a == Slot1) = Slot2 -- tau_1 (1)=1
         | (giPart a == Id) && (whichObject a == Slot2) = Slot1 -- tau_2 (0) = 0
         | (giPart a == Flip) && (whichObject a == Slot2) = Slot2 -- tau_2 (1) = 1
    sigmaInverse_2 a b = selectFirstMeetingCriteria allPossible sigma_2 b (head allPossible) where allPossible=[Groudit2by2{giPart= x,whichObject=a}| x <- [(toEnum y) :: CyclicGroup2 | y <- [0,1]]]
    tauInverse_2 a b = selectFirstMeetingCriteria allPossible tau_2 b (head allPossible) where allPossible=[Groudit2by2{giPart= x,whichObject=a}| x <- [(toEnum y) :: CyclicGroup2 | y <- [0,1]]]
    sigma1_2 a = (whichObject a,sigma_2 a)
    tau1_2 a = (whichObject a,tau_2 a)
    sigma2_2 (a,b) = sigmaInverse_2 a b
    tau2_2 (a,b) = tauInverse_2 a b
    biunitary_2 a = tau2_2 $ mySwap $ sigma1_2 a

-- a grouBit is also a grouDit
instance (FiniteGroudit2 a) => (FiniteGroudit a ObjectSet) where
    sigma = sigma_2
    tau = tau_2
    sigmaInverse = sigmaInverse_2
    tauInverse = tauInverse_2
    sigma1 = sigma1_2
    sigma2 = sigma2_2
    tau1 = tau1_2
    tau2 = tau2_2
    biunitary (x,y) = (z , sourceObject_2 z) where z=biunitary_2 x