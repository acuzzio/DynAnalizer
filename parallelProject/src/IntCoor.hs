module IntCoor where 

import Control.Applicative

newtype Vec a = Vec {runVec :: [a]} deriving (Show, Read, Eq)

instance Functor Vec where
  fmap f (Vec v) = Vec $ f `fmap` v

instance Applicative Vec where
  pure x = Vec $ (repeat x)
  Vec fs <*> Vec xs = Vec $ Prelude.zipWith (\f x -> f x) fs xs

instance Num a => Num (Vec a) where
  (+)         = liftA2 (+) 
  (-)         = liftA2 (-) 
  (*)         = liftA2 (*) 
  abs         = liftA abs 
  signum      = liftA signum
  fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (Vec a) where
   (/)  = liftA2 (/) 
   recip v = recip <$> v
   fromRational = pure . fromRational

instance (Floating a) => Floating (Vec a) where
  pi = pure pi
  exp v = liftA exp v
  log v = liftA exp v
  sqrt v = liftA sqrt v
  (**)   = liftA2 (**)
  sin v =  sin <$> v
  cos v =  cos <$> v
  tan v =  tan <$> v
  asin v = asin <$> v
  acos v = acos <$> v
  atan v = atan <$> v
  sinh v = sinh <$> v
  cosh v = cosh <$> v
  tanh v = tanh <$> v
  asinh v = asinh <$> v
  acosh v = acosh <$> v
  atanh v = atanh <$> v

toVec :: [a] -> Vec a
toVec x = Vec x

-- ============> Methods <============

vecdot :: Num a => Vec a -> Vec a -> a
v1 `vecdot` v2 =  sum . runVec $ v1 * v2

vecCross :: Num a => Vec a -> Vec a -> Vec a
v1' `vecCross` v2' = let [[x,y,z],[u,v,t]]= fmap runVec [v1',v2']
                     in Vec $ [(y*t-v*z),(u*z-x*t),(x*v-u*y)]

vecscal :: Num a => a -> Vec a -> Vec a
x `vecscal` vec = (pure x) * vec

vecnorm :: Vec Double -> Double
vecnorm v =  sqrt  $  v `vecdot` v

-- =================> BOND, ANGLE AND DIHEDRAL <=========

bond :: [Vec Double] -> Double
bond [p1,p2] = vecnorm $ p1 - p2

angle :: [Vec Double] -> Double
angle [p1,p2,p3] = let 
      ab = p1 - p2
      bc = p3 - p2
      numerator = vecdot ab bc
      denominat = (vecnorm ab) * (vecnorm bc) 
      fromRadToGrad = 180.0/pi
      gradiant  = acos (numerator/denominat)
      in fromRadToGrad * gradiant

dihedral :: [Vec Double] -> Double
dihedral [p1,p2,p3,p4] =
  let [xba,xca,xcb,xdb] = zipWith (-) [p2,p3,p3,p4] [p1,p1,p2,p2]
      [w1,w2] = zipWith vecCross [xba,xcb] [xca,xdb]
      [n1,n2] = map vecnorm [w1,w2]
      teta = (180.0/pi*) . acos $ ((w1 `vecdot` w2) / (n1*n2))
        in case 0.0 > (signum $ w2 `vecdot` xba) of
                True -> -teta
                False -> teta
