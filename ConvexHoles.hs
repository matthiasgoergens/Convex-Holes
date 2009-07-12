{-# OPTIONS -XScopedTypeVariables #-}
module Main where
import Data.List.Split
import Data.Ratio
import Control.Monad.Writer
import Control.Monad
import Data.List
import Data.Foldable (toList)
import Data.Function

import System
import qualified Data.Set as S
import Control.Monad.State.Strict
import Test.QuickCheck
import Control.Applicative
import Data.Maybe
import Halbebenen
import ATools

import Debug.Trace
--trace = flip const

areaL2 :: Lines -> Maybe Float
areaL2 [] = Just 0
areaL2 [_] = Just 0
areaL2 l = liftM (sum) . sequence $ zipWith areaTrapez2 (init l) (tail l)
areaTrapez2 a b
    = do (Point2 (x0) (y0)) <- mkPoint2 a
         (Point2 ( x1) (y1)) <- mkPoint2 b
         return $ (x1 - x0) * (y0+y1)

areaCPoly2 :: CPoly -> Maybe Float
areaCPoly2 (CPoly lower upper) = do al <- areaL2 lower
                                    au <- areaL2 upper
                                    return (abs (al - au))

range = (-1000,1000)
(rI,rS) = range
maxA2 = 2 * fromIntegral (rS-rI) ^ 2

areaSup2 :: Poly -> Maybe Float
areaSup2 p@(Poly lower@(l@(Point lx ly lz):_) upper@(u@(Point ux uy uz):_) inf sup)
    = case meet2 inf sup of
        -- Solte noch pruefen, ob die beiden (was?)
        (Point _ _ 0) -> Just maxA2
        m@(Point x y z)
            -> if m `linksVon` l /= GT || m `linksVon` u /= GT
               then Just maxA2
               else areaCPoly2 (CPoly (m:lower) (m:upper))

data TryAdd = TryAdd (Point -> Poly -> Poly)
instance Show TryAdd where
    show (TryAdd _) = "TryAdd <fn>"
instance Arbitrary TryAdd where
    arbitrary = oneof [return . TryAdd $ tryAddLower, return . TryAdd $ tryAddUpper]
    shrink _ = [TryAdd tryAddLower, TryAdd tryAddUpper]

instance Arbitrary Poly where
     arbitrary = do links <- arbitrary
                    oneof [sized (tryAddN (openNew links))
                          ,triang]
         where tryAddN poly 0 = return poly
               tryAddN poly n = do (TryAdd add) <- arbitrary
                                   p <- arbitrary
                                   tryAddN (add p poly) (n-1)
               triang = do links <- arbitrary
                           (Point x y z) <- arbitrary
                           let p = Point (abs x) (abs y) 1
                               pn = Point (abs x) (0 - abs y) 1
                           sized (tryAddN $ tryAddLower (links + pn) $ tryAddUpper (links + p) $ openNew links)
                           


rechts :: Poly -> Point
rechts (Poly lower upper _ _) = maximumBy linksVon $ lower ++ upper
-- links :: Poly -> Point
-- links (Poly lower upper _ _) = mininumBy linksVon $ lower upper

tryAddLower :: Point -> Poly -> Poly
tryAddLower pN poly@(Poly lower@(l:_) upper@(u:_) inf sup)
    = if rechts poly `linksVon` pN < GT
      then case (inside pN inf, inside pN sup) of
             (True, True) -> (Poly (pN:lower) upper infN sup)
             _ -> poly
      else poly
    where infN = makeH l pN
          supN = makeH pN u

tryAddUpper :: Point -> Poly -> Poly
tryAddUpper pN poly@(Poly lower@(l:_) upper@(u:_) inf sup)
    = if rechts poly `linksVon` pN < GT
      then case (inside pN inf, inside pN sup) of
             (True, True) -> (Poly lower (pN:upper) inf supN)
             _ -> poly
      else poly
    where infN = makeH l pN
          supN = makeH pN u

--       then Nothing
--       else Just (Poly (p:lower) upper infN sup)
--     where infN = makeR l p
--           p@(Point px py) = Point (lx + abs dx) y
          
-- tryAddUpper :: Point -> Poly -> Poly
-- tryAddUpper (Point dx y) poly@(Poly lower upper@(u@(Point ux uy):_) inf sup)
--     = if  px < ux || (sup < supN)
--       then Nothing
--       else Just (Poly l (p:upper) inf supN)
--     where supN = makeR l p 
--           p@(Point px py) = Point (lx + abs dx) y

prop_area p = areaSup2 p >= areaCPoly2 (close p)


type Lines = [Point]

data Poly = Poly !Lines !Lines
                 !Halbebene !Halbebene deriving (Ord, Eq)

instance Show CPoly where
    show (CPoly l u) = "CPoly " ++ (concat . map show . reverse $ u) ++ "\n\t" ++ (concat . map show . reverse $ l)


instance Show Poly where
    show (Poly l u inf sup) = "\nPoly " ++ (concat . map show . reverse $ u) ++ "\n\t" ++ (concat . map show . reverse $ l) ++
                                "\n\t"++show sup ++ "\n\t" ++ show inf
--                              ++ "\n\tm: "++show (meet (last l) inf (last u) sup)
    
data CPoly = CPoly {l :: !Lines, u :: !Lines}
           deriving (Ord, Eq)

openNew :: Point -> Poly
openNew l = (Poly [links] [links]
             (makeH links (links - Point 0 1 1))
             (makeH links (links - Point 0 1 1)))
    where links = normalize l
--                           Runter  Hoch)

prop_openNewL p = inside p' hL 
    where (Poly _ _ hL hU) = openNew p
          p' = p + mkPointI (1, 0)
prop_openNewLtriv p = inside p' hL 
    where (Poly _ _ hL hU) = openNew p
          p' = p + mkPointI (1, 0)
prop_openNewU p = inside p' hU
    where (Poly _ _ hL hU) = openNew p
          p' = p + mkPointI (1, 0)
prop_openNewUtriv p = inside p hU
    where (Poly _ _ hL hU) = openNew p
          p' = p + mkPointI (1, 0)


-- WriterT Trace (State z) a



examine :: Poly -> (State Float) (Maybe Poly)
examine p = do bestSeen <- get
               (if fromJust (areaSup2 p) < bestSeen
                then return Nothing
                else do modify (max (fromJust $ areaCPoly2 (close p)))
                        return $ Just p)

-- convert to dfs! -> weniger RAM

treatScan1o1 :: Point -> Poly -> (State Float) [Poly]
treatScan1o1 pN@(Point xN yN zN) polyO@(Poly lower@(l:_) 
                                        upper@(u:_)
                                        inf
                                        sup)
    = if True -- isOpenAt xN $ polyO
      then (sequence . map examine $ polyN)
           >>= return . catMaybes
      else do let c = close polyO
              {- trace ("closed:\t"++show c++"\narea:\t"++show (areaCPoly c)) $ -}
              -- tell [c]
              modify (max (fromJust $ areaCPoly2 c))
              return $ []
    where 
          infN = makeH l pN
          supN = makeH pN u
          rangeUnten = Point xN (rI*zN) zN
          rangeOben = Point xN (rS*zN) zN

          polyN = if inside rangeUnten sup && inside rangeOben inf
                  then case (inside pN inf, inside pN sup) of
                         (True, True) -> [Poly lower (pN:upper) inf supN
                                         ,Poly lower     upper  inf supN
                                         ] ++
                                        [Poly (pN:lower) upper infN sup
                                        ,Poly     lower  upper infN sup
                                        ]
                         (False, False) -> {- trace ("No continuation for "++show polyO ++ " at " ++ show pN) $-} []
                         _ -> {-trace ("komplett oben oder untenrum") $ -} [polyO]
                  else []
                         
close :: Poly -> CPoly
close polyO@(Poly lower@(l:_)
                  upper@(u:_)
                  _         _)
    = case compare l u of
        LT -> CPoly (u:lower) upper
        EQ -> CPoly lower upper
        GT -> CPoly lower (l:upper)

prop_close left (NonEmpty lo) (NonEmpty up) = l == u
    where (CPoly (l:_) (u:_)) = close (Poly (map mkPointI lo ++ [mkPointI left]) (map mkPointI up ++ [mkPointI left]) undefined undefined)


runDeep1 :: [Point] -> Poly -> (State Float) ()
runDeep1 [] cur = do examine cur
                     return ()
runDeep1 (pN:rest) cur = do polyN <- treatScan1o1 pN cur
                            mapM_ (runDeep1 rest) $  polyN
--                                  trace ("#polys:\t" ++ show (length polyN) ++"\tpn: "++show pN) 
                                           


run1p :: Point -> [Poly] -> (State Float) [Poly]
                             -- Writer [Poly] (S.Set Poly)
run1p pN polys = liftM (concat)
                 . sequence . fmap (treatScan1o1 pN)
--                 . trace ("#polys:\t" ++ show (length polys) ++"\tpn: "++show pN)
                 $ polys

--                 >>= liftM ((:) (openNew pN))

runDeep :: [Point] -> ((), Float)
runDeep ps = (runState (mapM_ doAll (init . tails . nub . sortBy linksVon $ ps))
                       (-1000))
    --runWriter . doAll
      where doAll :: [Point] -> (State Float) ()
            doAll (links:rest) = do best <- get
                                    let xyzzy k = trace ("length:\t" ++ show (length rest)++"\thigh:\t" ++ show best) k
                                    (xyzzy runDeep1 rest (openNew links))
                                    
                where 
run = runDeep

runBreadth :: [Point] -> ((), Float)
runBreadth ps = (runState (mapM_ doAll (init . tails . nub . sortBy linksVon $ ps))
                 (-999))
    --runWriter . doAll
    where doAll :: [Point] -> (State Float) ()
          doAll (links:rest) = do open <- foldl (>>=) (return $ [openNew links]) . map run1p $ rest
                                  modify (t . max (maximum (map (fromJust . areaCPoly2 . close) open)))
                                  return ()
              where t s = trace ("length:\t" ++ show (length rest)++"\thigh:\t" ++ show s) s


allClose = map close . uncurry (++)

bestCPoly :: [CPoly] -> Float --[Float]
bestCPoly cp = maximum . map (fromJust . areaCPoly2) $ cp


assoc f l = zip l (map f l)
assoc1 f l = zip (map f l) l

s0 :: Integer
s0 = 290797

f s = (s^2) `mod` 50515093



t = map p . splitEvery 2 . map c . tail . iterate f
    where c s = (s `mod` 2000) - 1000
          p [b,a] = Point (id . fromIntegral $ a) (fromIntegral $ b) 1

t1 = map mkPointI $
     [(0,01)
     ,(1,1)
     ,(1,-1)
     ,(2,0)
     ,(2,0)
     ,(3,0)
     ]

t2 = map mkPointI $
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



bestC t = let ((),bestClosed) = run $ t
              -- bestOpen = bestCPoly $ map close (toList o)
          in bestClosed

mkPoints xy = t
    where (x,y) = unzip xy
          t = map mkPointI $ zip (map fromIntegral x) (map fromIntegral y)

prop_bestCX (NonEmpty xy) = bestC t == bestC (map negateX t)
    where t = mkPoints xy

prop_bestCY (NonEmpty xy) = bestC t == bestC (map negateY t)
    where t = mkPoints xy
prop_bestCXY (NonEmpty xy) = bestC t == bestC (map (negateX . negateY) t)
    where t = mkPoints xy

doT t = do let (_,bestClosed) = run $  t
--               bestOpen = bestCPoly $ map close (toList o)
           putStrLn "-------"
           putStr "bestClosed:\t"
           print bestClosed
--           putStr "open:\t"
--           print (assoc (areaCPoly2 . close) . toList $ o)
--           putStr "bestOpen:\t"
--           print bestOpen
--           putStr "best:\t"
           putStrLn $ show $ bestClosed 
           putStrLn $ show $ bestClosed / 2

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


tx = mkPoints px
px=[
    (3,1) ,(4,1)

   ,(2,2)
   ,(0,0),       (1,0), (2,0)]

py = [(0,3)

    ,(0,1),(1,1)
               ,(2,0)]

pz = [(0,0),(1,0),(0,1)]

pa = [(2,0),(1,1),(0,0)]


