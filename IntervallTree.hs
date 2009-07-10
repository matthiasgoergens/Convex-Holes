module IntervallTree where
import Test.QuickCheck
import Data.List (sort)
import Control.Monad
import qualified Data.Set as S

data Interval a = Closed a a
                  deriving (Eq, Ord, Show)

mkClosed a b = Closed l r
    where [l,r] = sort [a, b]

data ITree k v = Node (Interval k) (S.Set v) (ITree k v) (ITree k v)
               | Leaf deriving Show

instance (Arbitrary a, Ord a) => Arbitrary (Interval a) where
    arbitrary = oneof [--liftM LOpen arbitrary
                      liftM2 mkClosed arbitrary arbitrary]
                      --liftM ROpen arbitrary]

instance (Arbitrary k, Ord k, Num k, Arbitrary v, Ord v) =>
    Arbitrary (ITree k v) where
        arbitrary = liftM fromList arbitrary

fromList :: (Num k, Ord k, Ord v) => [(Interval k,v)] -> ITree k v
fromList ivs = foldr ($) emptyT (zipWith insert is vs)
    where (is, vs) = unzip ivs

prop_insert1 i v = look i (insert i v emptyT) == S.singleton v
    where types :: (Interval Int, Int)
          types = (i,v)

prop_insertN ivs = map (flip look tree) is == map S.singleton vs
    where types :: [(Interval Int, Int)]
          types = (ivs)
          tree = fromList ivs
          (is,vs) = unzip ivs

insert :: (Ord k, Num k, Ord v) =>
          Interval k -> v -> ITree k v -> ITree k v
insert i v Leaf = ITree i (S.singleton v) Leaf Leaf
insert (Closed l r) v (Node (Closed l1 r1) s

prop_delete t i v = not . S.member v .
                    look i . delete i v $
                    t
    where types :: (Interval Int, Int)
          types = (i,v)

delete :: Interval k -> v -> ITree k v -> ITree k v
delete _ _ _ = undefined

deleteEverywhere :: v -> ITree k v -> ITree k v
deleteEverywhere  _ _ = undefined

look :: Interval k -> ITree k v -> S.Set v
look _ _ = undefined

emptyT :: ITree k v
emptyT = undefined
