{-# OPTIONS -XScopedTypeVariables #-}
module ATools where

import Test.QuickCheck
import Data.Ratio

instance Arbitrary Point where
    arbitrary = do (x::Integer) <- arbitrary
                   (y::Integer) <- arbitrary
                   (z::Integer) <- arbitrary
                   return $ normalize $ Point (fromIntegral x) (fromIntegral y) (fromIntegral z)
    shrink (Point x y z) = do x' <- shrink x
                              y' <- shrink y
                              z' <- shrink z
                              return $ normalize $ Point x' y' z'

scm a b = a * b `div` gcd a b

linksVon (Point ax _ az) (Point bx _ bz) 
    = compare (ax * bz) (bx * az)

(~*) :: Integer -> Point -> Point
(~*) a (Point x y z) = (Point (a*x) (a*y) z)

isFinite (Point _ _ 0) = False
isFinite _ = True

instance Num Point where
    (+) (Point x0 y0 0) (Point x1 y1 0)
        = Point (x0 + x1) (y0+y1) (0+0)
    (+) (Point x0 y0 z0) (Point x1 y1 z1)
        = normalize (Point ((x0*z1+x1*z0) `div` g) ((y0*z1+y1*z0) `div` g) z)
          where g = gcd z0 z1
                z = z0 * z1 `div` g

    (-) (Point x0 y0 0) (Point x1 y1 0)
        = Point (x0 - x1) (y0 - y1) (0+0)
    (-) (Point x0 y0 z0) (Point x1 y1 z1)
        = normalize (Point ((x0*z1-x1*z0) `div` g) ((y0*z1-y1*z0) `div` g) z)
          where g = gcd z0 z1
                z = z0 * z1 `div` g
    abs p
        = Point (abs x) y z
          where (Point x y z) = normalize p
    fromInteger x
        = Point (fromInteger x) 0 1
    signum p
           = (Point (signum x) y z)
        where (Point x y z) = normalize p

normalize p@(Point x y z) | z < 0 = Point (-x) (-y) (-z)
                          | otherwise = p

prop_norm p = p == normalize p
prop_addP p0 p1 = (p0 + p1) - p1 == p0

prop_addP2 p0 = (p0 + p0) == 2 ~* p0

data Point = Point {x :: !X, y :: !Y, z :: !Z} deriving Show
data Point2 = Point2 !Float !Float

mkPoint2 :: Point -> Maybe Point2
mkPoint2 (Point _ _ 0) = Nothing
mkPoint2 (Point x y z) = Just $ Point2 (fromRational (x % z)) (fromRational (y % z))

instance Ord Point where
    compare (Point xa ya za) (Point xb yb zb)
        = compare (xa * zb, ya * zb, za*zb) (xb * za, yb * za, zb*za)

instance Eq Point where
    (==) p0 p1 = compare p0 p1 == EQ


-- instance Show Point where
--     show (Point x y z) = "("++ sx ++ " " ++ sy ++ " " ++ sz ++ ")"
--         where sx = m0 $ show x
--               sy = m0 $ show y
--               sz = m0 $ show z
--               m0 s = case reverse s of
--                        '0':'.':_ -> reverse . drop 2 . reverse $ s
--                        _ -> s

type K = Integer
type X = K
type Y = K
type Z = K