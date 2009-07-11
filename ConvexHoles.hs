{-# OPTIONS -XScopedTypeVariables #-}
module Main where
import Data.List.Split
import Data.Ratio
import Control.Monad.Writer
import Control.Monad
import Data.List
import Data.Foldable (toList)
import Data.Function
--import Debug.Trace
import System
import qualified Data.Set as S
import Control.Monad.State.Strict
import Test.QuickCheck
import Control.Applicative
import Data.Maybe

trace = flip const

type X = Float
type Y = Float

data R = Runter | R !(Float) | Hoch
       deriving (Show, Eq, Ord)

makeR :: Point -> Point -> R
makeR (Point x0 y0) (Point x1 y1)
    = case compare x1 x0 of
        LT -> error "Nur vorwaerts!"
        EQ -> case compare y1 y0 of
                LT -> Runter
                EQ -> error "Points are equal!"
                GT -> Hoch
        GT -> R $ ((y1 - y0) / (x1 - x0))

data Interval x = LOpen x | Closed x x | Here x | UOpen x
                | LUOpen
                  deriving EQ

extrapolate :: Point -> R -> X -> Maybe Y
extrapolate (Point x0 y0) (R r) atX
    = Just ((atX - x0) * r + y0)
extrapolate _ Runter _ = Nothing
extrapolate _ Hoch _ = Nothing


-- meetScan _ (Ray (Point _ Oben) _)
--     = error "meetScan: Ausgang des Strahls ist Oben"
-- meetScan scan (Ray (Point x0 (Y y0)) (Point x1 Unten))
--     | scan == x0 && x0 == x1 
--         = (Unten, Y y0)
--     | otherwise = (Unten, Unten)
-- meetScan scan (Ray (Point x0 (Y y0)) (Point x1 Oben))
--     | scan == x0 && x0 == x1 
--         = (Y y0, Oben)
--     | otherwise = (Oben, Oben)

allowed :: X -> Point -> R -> Maybe (Interval Y)
allowed scan p@(Point x0 y0) r
    = case compare scan x0 
      LT -> error "allowed: Please only ask when scan > x0."
      EQ -> case r of
              Runter -> Just $ LOpen y0
              R _ -> Just $ Here y0
              Hoch -> Just $ UOpen y0
      GT -> do liftM Here $ extrapolate p r scan

infInt :: Interval Y -> Interval Y
infInt i@(Here x) = UOpen x
infInt i@(Closed l u) = UOpen l
infInt i@(LOpen y) = UOpen y
infInt i@(UOpen _) = i

supInt :: Interval Y -> Interval Y
infInt i@(Here x) = LOpen x
infInt i@(Closed l u) = LOpen u
infInt i@(LOpen y) = UOpen y
infInt i@(UOpen _) = i

combine :: (Interval Y) -> (Interval Y) -> (Interval Y)
combine lowerInter upperInter = 

prop_makeR p0@(Point x0 y0) p1@(Point x1 y1) 
    = (x0 < x1) && (y0 /= y1) ==>
       abs (y1 - y') < 0.001
    where r = makeR p0 p1
          Just y' = extrapolate p0 r x1


cmp :: Point -> R -> Point -> Ordering
cmp p0 r pn = compare (makeR p0 pn) r

instance Arbitrary Point where
    arbitrary = do (x::Integer) <- arbitrary
                   (y::Integer) <- arbitrary
                   return $ Point (fromIntegral x) (fromIntegral y)
    shrink (Point x y) = do x' <- shrink x
                            y' <- shrink y
                            return $ Point x' y'

data Point = Point {x :: !X, y :: !Y} deriving (Eq, Ord)

instance Show Point where
    show (Point x y) = "("++ sx ++ " " ++ sy ++ ")"
        where sx = m0 $ show x
              sy = m0 $ show y
              m0 s = case reverse s of
                       '0':'.':_ -> reverse . drop 2 . reverse $ s
                       _ -> s

areaL :: Lines -> Float
areaL [] = 0
areaL [_] = 0
areaL (p1:ps@(p2:_)) = areaTrapez p1 p2 + areaL ps
areaTrapez (Point (x0) (y0)) (Point ( x1) (y1))
    = (x1 - x0) * (y0+y1) / 2

areaCPoly :: CPoly -> Float
areaCPoly (CPoly lower upper) = areaL lower - areaL upper

range = (-1000,1000)
(rI,rS) = range
maxA = (rS-rI) ^ 2

areaSup :: Poly -> Float
areaSup p@(Poly lower@(l@(Point lx ly):_) upper@(u@(Point ux uy):_) inf sup)
    = case meet l inf u sup of
        Nothing -> maxA
        Just (m@(Point mx _))
            -> if mx <= lx || mx <= ux
               then maxA
               else areaCPoly (CPoly (m:lower) (m:upper))
data TryAdd = TryAdd (Point -> Poly -> Poly)
instance Arbitrary TryAdd where
    arbitrary = oneof [tryAddLower, tryAddUpper]

instance Arbitrary Poly where
    arbitrary = do links <- arbitrary
                   sized (tryAddN (openNew links))
        where tryAddN poly 0 = return poly
              tryAddN poly n = if not $ isOpenAt (rechts poly) poly
                               then return poly
                               else do (TryAdd add) <- arbitrary
                                       p <- arbitrary
                                       case add p poly of
                                         Nothing -> tryAddN poly n
                                         Just poly' -> tryAddN poly' (n-1)

rechts :: Poly -> X
rechts (Poly lower upper _ _) = max `on` (x . head)
links :: Poly -> X
links (Poly lower upper _ _) = min `on` (x . last)

addable :: Point -> Poly -> Bool
addable p@(Point x y) (Poly lower@(l:_) upper@(u:_) inf sup)
    = inf <= infN && supN <= sup
    where infN = makeR l p
          supN = makeR u p

tryAddLower :: Integer -> Integer -> Poly -> Poly
tryAddLower (Point dx y) poly@(Poly (lower@(l@(Point lx ly):_)) upper inf sup)
    = if  px < lx || (infN < inf)
      then Nothing
      else Just (Poly (p:lower) upper infN sup)
    where infN = makeR l p
          p@(Point px py) = Point (lx + abs dx) y
          
          

tryAddUpper :: Point -> Poly -> Poly
tryAddUpper (Point dx y) poly@(Poly lower upper@(u@(Point ux uy):_) inf sup)
    = if  px < ux || (sup < supN)
      then Nothing
      else Just (Poly l (p:upper) inf supN)
    where supN = makeR l p 
          p@(Point px py) = Point (lx + abs dx) y

prop_area p = areaSup p >= areaCPoly (close p)




type Lines = [Point]

--data Poly = Poly {lower :: Lines ,upper :: Lines
--                 ,inf   :: Ray   ,sup   :: Ray}
data Poly = Poly !Lines !Lines
                 !R !R deriving (Ord, Eq)

instance Show CPoly where
    show (CPoly l u) = "CPoly " ++ (concat . map show . reverse $ u) ++ "\n\t" ++ (concat . map show . reverse $ l)


instance Show Poly where
    show (Poly l u inf sup) = "\nPoly " ++ (concat . map show . reverse $ u) ++ "\n\t" ++ (concat . map show . reverse $ l) ++
                                "\n\t"++show sup ++ "\n\t" ++ show inf ++
                                "\n\tm: "++show (meet (last l) inf (last u) sup)
    
meet :: Point -> R -> Point -> R -> Maybe Point
meet p0@(Point x0 y0) (R r0) p1@(Point x1 y1) (R r1)
    | r0 == r1 = Nothing
    | otherwise = Just $ Point x y
    where x = (x1*r1 - x0*r0 + y0 - y1) / (r1-r0)
          y = r0 * (x - x0) + y0
meet _ _ _ _ = Nothing

data CPoly = CPoly {l :: !Lines, u :: !Lines}
           deriving (Ord, Eq)

openNew :: Point -> Poly
openNew links@(Point x y) = (Poly [links] [links]
                             Runter
                             Hoch)


-- WriterT Trace (State z) a



isOpenAt scan poly@(Poly (l@(Point lx ly):_)
                    (u@(Point ux uy):_)
                    inf
                    sup)
    = case compare scan (rechts poly) of
        LT -> True
        EQ -> if (lx == ux) 
              then abs (uy - ly) > 1
              else True
        GT -> case (inf, sup) of
             (_, Runter) -> False
             (Hoch, _) -> False
             (R _, R _)
                 -> fromMaybe (error "Should not happen (isClosedAt)") $
                    do syInf <- liftM floor $ extrapolate l inf scan
                       sySup <- liftM ceiling $ extrapolate u sup scan
                       return (syInf < sySup)
             _ -> True

examine :: Poly -> WriterT [CPoly] (State Float) (Maybe Poly)
examine p = do bestSeen <- get
               (if areaSup p < bestSeen
                then return Nothing
                else do modify (max (areaCPoly (close p)))
                        return $ Just p)
                               

treatScan1o1 :: Point -> Poly -> WriterT [CPoly] (State Float) [Poly]
treatScan1o1 pN@(Point xN yN) polyO@(Poly lower@(l:_) 
                                          upper@(u:_)
                                          inf
                                          sup)
    = if True -- isOpenAt xN $ polyO
      then (sequence . map examine . {- nub . -} sort $ untenlang ++ obenlang)
           >>= return . catMaybes
      else do let c = close polyO
              {- trace ("closed:\t"++show c++"\narea:\t"++show (areaCPoly c)) $ -}
              tell [c]
              modify (max (areaCPoly c))
              return $ []
    where 
          infN = makeR l pN
          supN = makeR u pN

          untenlang
              = if supN <= sup
                then [Poly lower (pN:upper) inf supN
                     ,Poly lower     upper  inf supN
                     ]
                else trace ("No untenlang for "++show polyO ++ " at " ++ show pN) $ []
          obenlang 
              = if inf <= infN
                then [Poly (pN:lower) upper infN sup
                     ,Poly     lower  upper infN sup
                     ]
                else trace ("No obenlang for "++show polyO ++ " at " ++ show pN) $ []


close :: Poly -> CPoly
close polyO@(Poly lower@(l:_)
                  upper@(u:_)
                  _         _)
    = case compare l u of
        LT -> CPoly (u:lower) upper
        EQ -> CPoly lower upper
        GT -> CPoly lower (l:upper)

prop_close left (NonEmpty lo) (NonEmpty up) = l == u
    where (CPoly (l:_) (u:_)) = close (Poly (map mkPoint lo ++ [mkPoint left]) (map mkPoint up ++ [mkPoint left]) undefined undefined)

mkPoint  = uncurry Point

run1p :: Point -> [Poly] -> WriterT [CPoly] (State Float) [Poly]
                             -- Writer [Poly] (S.Set Poly)
run1p pN polys = liftM ((openNew pN:) . concat)
                 . sequence . fmap (treatScan1o1 pN)
                 . trace ("#polys:\t" ++ show (length polys) ++"\tpn: "++show pN++ "\tpolys:\t"++show polys++"\n-+-+-\n")
                 $ polys

--                 >>= liftM ((:) (openNew pN))


run :: [Point] -> (([Poly], [CPoly]), Float)
run ps = (runState (runWriterT (doAll ps)) (-999))
    --runWriter . doAll
      where doAll :: [Point] -> WriterT [CPoly] (State Float) [Poly]
                     -- Writer [Poly] (S.Set Poly)
            doAll = foldl (>>=) (return $ []) . map run1p . nub . sort

allClose = map close . uncurry (++)

bestCPoly :: [CPoly] -> (Float) --[Float]
bestCPoly cp = maximum . map (areaCPoly) $ cp


assoc f l = zip l (map f l)
assoc1 f l = zip (map f l) l

s0 :: Integer
s0 = 290797

f s = (s^2) `mod` 50515093



t = map p . splitEvery 2 . map c . tail . iterate f
    where c s = (s `mod` 2000) - 1000
          p [a,b] = Point (id . fromIntegral $ a) (fromIntegral $ b)

t1 = map mkPoint $
     [(0,0)
     ,(1,1)
     ,(1,-1)
     ,(2,0)
     ,(2,0)
     ,(3,0)
     ]

t2 = map mkPoint $
     [
--      (-883, -38) 
--     (-827, -607) 
--     ,(-811, 286) 
--     (-754, 70) 
     (-665, -726) 
     ,(-646, -422) 
     ,(-488, 732) -- kaputt
     ,(-477, -23) 
     ,(-454, -947) 
     ,(-389, 938) 
     ,(-178, 540)  -- kaputt
     ,(-60, 138) 
     ,(273, -175) 
     ,(913, -860) ]
--     [(-883, -38) ,(-827, -607) ,(-811, 286) ,(-754, 70) ,(-665, -726) ,(-646, -422) ,(-488, 732) ,(-477, -23) ,(-454, -947) ,(-389, 938) ,(-178, 540) ,(-60, 138) ,(54, 316) ,(273, -175) ,(331, 698) ,(491, 368) ,(527, 144) ,(806, 528) ,(913, -860) ]

negateX (Point x y) = (Point (-x) y)
negateY (Point x y) = (Point (x) (-y))
negateXY = negateX . negateY

bestC t = let ((o,c),bestClosed) = run $  t
              bestOpen = bestCPoly $ map close (toList o)
          in max bestClosed bestOpen 

mkPoints xy = t
    where (x,y) = unzip xy
          t = map mkPoint $ zip (map fromIntegral x) (map fromIntegral y)

prop_bestCX (NonEmpty xy) = bestC t == bestC (map negateX t)
    where t = mkPoints xy

prop_bestCY (NonEmpty xy) = bestC t == bestC (map negateY t)
    where t = mkPoints xy
prop_bestCXY (NonEmpty xy) = bestC t == bestC (map (negateX . negateY) t)
    where t = mkPoints xy

doT t = do let ((o,c),bestClosed) = run $  t
               bestOpen = bestCPoly $ map close (toList o)
           putStrLn "-------"
           putStr "bestClosed:\t"
           print bestClosed
--           putStr "open:\t"
--           print (assoc (areaCPoly . close) . toList $ o)
           putStr "bestOpen:\t"
           print bestOpen
           putStr "best:\t"
           putStrLn $ show $ max bestClosed bestOpen 

main = do {- putStrLn $ unlines $ map show $ c
          putStrLn "---"
          
          putStrLn $ unlines $ map show $ o
          putStrLn "---"
          putStrLn $ unlines $ map show $ allClose (o,c)-}
--          putStrLn "---"
          args <- getArgs
          let n = read (args !! 0)
              t2 = take n (t s0)
              t' = tx
          doT t2
--          putStrLn "negated:"
--          doT (map negateX t')

--          print $ sort t'
          
    where 
          t' = undefined
          _ = t2

t3 = mkPoints [      (1,-1)
              ,(0,0),(1,0)
                    ,(1,1)]
    --mkPoints [(9,0),(9,1),(9,2),(0,0)]

-- p00
-- p1n
-- 1

-- Poly (0 Y 0)(1 Y (-1.0))
--        (0 Y 0)(1 Y 0)
--        [(0 Y 0) -> (1 Y (-1.0))]
--        [(0 Y 0) -> (1 Y 0)]
--        m: Just (0 Y (-0.0)),0.5)


tx = mkPoints z
x=[
--          {-,(3,1)-} ,(4,1)

                       (2,2)
   ,(0,0),       (1,0), (2,0)]

y = [(0,3)

    ,(0,1),(1,1)
               ,(2,0)]

z = [(0,0),(1,0),(1,1)]


