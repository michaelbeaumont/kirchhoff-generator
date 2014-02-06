{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs, KindSignatures #-}


module Types where
import System.Random
import Data.List
import Control.Monad.Identity
import Control.Monad.State
import Data.Maybe (mapMaybe)

-- VOLTAGE,CURRENT SYNONYMS
type Voltage = Double
type Current = Double

-- HELPER FUNCTION
foldWithMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldWithMaybe f acc (a:as) = f acc a >>= \x -> foldWithMaybe f x as
foldWithMaybe _ acc [] = Just acc

-- RESISTANCE TYPES
data Resistance' a = Value a | Inf | Null deriving Show
instance Functor Resistance' where
    fmap f (Value v) = Value (f v)
    fmap _ Inf = Inf
    fmap _ Null = Null

instance (Eq a) => Eq (Resistance' a) where
    (==) (Value v) (Value w) = v == w
    (==) Null Null = True
    (==) Inf Inf = True
    (==) _ _ = False


--these are monoid instances
pappend :: Resistance -> Resistance -> Resistance
pappend (Value p) (Value q) = Value (p*q / (p+q))
pappend Null _ = Null
pappend _ Null = Null
pappend v Inf = v
pappend Inf v = v

sappend :: Resistance -> Resistance -> Resistance
sappend (Value p) (Value q) = Value (p+q)
sappend Inf _ = Inf
sappend _ Inf = Inf
sappend v Null = v
sappend Null v = v

type Resistance = Resistance' Double

-- DIRECTION TYPES (is this block in or opposite its normal position?)
data Direction = Forward | Backward deriving Show

spin :: Direction -> Direction
spin Forward = Backward
spin Backward = Forward

instance Eq Direction where
    (==) Forward Backward = False
    (==) Backward Forward = False
    (==) _ _ = True


-- POWER SOURCE STUFF
data Source :: * where
    VoltageSource :: Double -> Source
    CurrentSource :: Double -> Source
    deriving Show

class ReduceFail mf where
    (<?>) :: mf -> mf -> Maybe mf

instance ReduceFail Source where
    (<?>) (VoltageSource a)
        (VoltageSource b) = if a == b
                              then Just $ VoltageSource b
                              else Nothing
    (<?>) (CurrentSource a)
        (CurrentSource b) = if a == b
                              then Just $ CurrentSource b
                              else Nothing
    (<?>) _ _ = Nothing

-- COMPONENT STUFF (sources and resistors)
data Component = Resistor
                 {
                   rDir :: !Direction,
                   rRes :: !(Resistance' Int),
                   rVol :: !Voltage,
                   rCur :: !Current
                 }
               | Source Source
               deriving Show


instance ReduceFail Component where
    (<?>) (Resistor Forward r1 v1 c1)
        (Resistor Backward r2 v2 c2) = if r1 == r2
                                then Just $ Resistor Forward r1 (v1-v2) (c1-c2)
                                else Nothing
    (<?>) (Resistor Backward r1 v1 c1)
        (Resistor Forward r2 v2 c2) = if r1 == r2
                                then Just $ Resistor Forward r1 (v2-v1) (c2-c1)
                                else Nothing
    (<?>) (Resistor Backward r1 v1 c1)
        (Resistor Backward r2 v2 c2) = if r1 == r2
                                then Just $ Resistor Backward r1 (v1+v2) (c1+c2)
                                else Nothing
    (<?>) (Resistor Forward r1 v1 c1)
        (Resistor Forward r2 v2 c2) = if r1 == r2
                                then Just $ Resistor Forward r1 (v1+v2) (c1+c2)
                                else Nothing
    (<?>) (Source a) (Source b) = a <?> b >>= Just . Source
    (<?>) _ _ = Nothing

-- COMPONENT BLOCKS
data Block = Block
             {
               parallel :: !Bool,
               _dir :: !Direction,
               _size :: !Int,
               _res :: !Resistance,
               _vol :: !Double,
               _cur :: !Double } deriving Show


--resistance is undefined after we combine two blocks
--set to Null?
instance ReduceFail Block where
    (<?>) (Block p1 Forward s1 r1 v1 c1) (Block p2 Backward s2 r2 v2 c2) =
        if s1 == s2 && p1 == p2
            then Just $ Block p1 Forward s1 r1 (v1-v2) (c1-c2)
            else Nothing
    (<?>) (Block p1 Backward s1 r1 v1 c1) (Block p2 Forward s2 r2 v2 c2) =
        if s1 == s2 && p1 == p2
            then Just $ Block p1 Forward s1 r1 (v2-v1) (c2-c1)
            else Nothing
    (<?>) (Block p1 d1 s1 r1 v1 c1) (Block p2 d2 s2 r2 v2 c2) =
        if s1 == s2 && p1 == p2 && d1 == d2
            then Just $ Block p1 d1 s1 r1 (v1+v2) (c1+c2)
            else Nothing

-- ROSE TREE STUFF
data RoseTree a b = RoseTree {
                           treeBlock :: a,
                           treeChildren :: [RoseTree a b]
                             }
                  | RoseNode { component :: b }
                  | TopNode { component :: b } deriving Show

--unused
instance Functor (RoseTree a) where
    fmap f (RoseTree bl s) = RoseTree bl (map (fmap f) s)
    fmap f (RoseNode comp) = RoseNode (f comp)
    fmap f (TopNode comp) = TopNode (f comp)

data RoseTreeCxt a b = In (RoseTreeCxt a b) a [RoseTree a b] [RoseTree a b]
                     | TopWith b deriving Show

type RoseZipper a b = (RoseTree a b, RoseTreeCxt a b)

type Netz a b = (RoseTree a b, b)

netzToZipper :: Netz a b -> RoseZipper a b
netzToZipper (t, top) = (t, TopWith top)

petalDown :: RoseZipper a b -> Maybe (RoseZipper a b)
petalDown (RoseTree v (c:cs), cxt) = Just (c, In cxt v [] cs)
petalDown _ = Nothing

petalUp :: RoseZipper a b -> Maybe (RoseZipper a b)
petalUp (t, In cxt v ls rs) = Just (RoseTree v (reverse ls ++ t:rs), cxt)
petalUp _ = Nothing

petalLeft :: RoseZipper a b -> Maybe (RoseZipper a b)
petalLeft (t, In cxt v (l:ls) rs) = Just (l, In cxt v ls (t:rs))
petalLeft _ = Nothing

petalRight :: RoseZipper a b -> Maybe (RoseZipper a b)
petalRight (t, In cxt v ls (r:rs)) = Just (r, In cxt v (t:ls) rs)
petalRight _ = Nothing


-- CIRCUIT SPECIFIC ROSE TREE STUFF
type CompTree = RoseTree Block Component

compTree p d s r v c sub = RoseTree (Block p d s r v c) sub

type CompZipper = RoseZipper Block Component

type CompNetz = Netz Block Component

--USEFUL METHODS
size :: CompTree -> Int
size (RoseTree (Block _ _ s _ _ _) _) = s
size (RoseNode _ ) = 1
size (TopNode _ ) = 1

resistance :: CompTree -> Resistance
resistance (RoseNode (Resistor _ r _ _)) = fmap fromIntegral r
resistance (RoseNode (Source (VoltageSource _))) = Null
resistance (RoseNode (Source (CurrentSource _))) = Inf
resistance (TopNode (Resistor _ r _ _)) = fmap fromIntegral r
resistance (TopNode (Source (VoltageSource _))) = Null
resistance (TopNode (Source (CurrentSource _))) = Inf
resistance (RoseTree (Block _ _ _  r _ _) _) = r


sumP :: [CompTree] -> Resistance
sumP = foldr (\a b -> resistance a `pappend` b) Inf

sumS :: [CompTree] -> Resistance
sumS = foldr (\a b -> resistance a `sappend` b) Null


findSourceZippers :: Maybe CompZipper -> [CompZipper]
findSourceZippers Nothing = []
findSourceZippers (Just z@(RoseNode _, _))
    | holeIsSource z = z : rest
    | otherwise = rest
    where rest = findSourceZippers (petalRight z)
          holeIsSource (RoseNode (Source _), _) = True
          holeIsSource _ = False

findSourceZippers (Just z@(TopNode _, _)) =
    findSourceZippers (petalRight z)
findSourceZippers (Just z@(RoseTree _ _, _)) =
    findSourceZippers (petalDown z) ++ findSourceZippers (petalRight z)


findAllSources :: CompNetz -> [CompZipper]
findAllSources = findSourceZippers . Just . netzToZipper

-- FUNCTIONS TO DEAL WITH SINGLE NETZ
--



-- Zip a tree from a point back upwards
--       5                            4
--      / \   (lift from 4)           |
--     2   7   ----------->           2
--    / \                            /|
--   1   4                          1 5
--                                     \
--                                      7
zipUp' :: (Component -> CompTree) 
          -> (Bool, Bool)
          -> RoseTreeCxt Block Component 
          -> CompTree
zipUp' f (parentPar, didFlip) (In cxt (Block p d _ _ _ _) ls rs) = RoseTree newInfo newChildren
    where willFlip = if p == parentPar
                        then didFlip
                        else not didFlip
          flipper = case willFlip of
                        True -> flipBlock
                        False -> id
          newMid = zipUp' f (p, willFlip) cxt
          newChildren = map flipper (reverse ls) ++ newMid : map flipper rs
          newSize = sum (map size newChildren)
          addRes True = sumP newChildren
          addRes False = sumS newChildren
          newInfo = Block p d newSize (addRes p) 0 0
zipUp' f _ (TopWith top) = f top

--The method we actually use
zipUp :: (Bool, Bool)
          -> RoseTreeCxt Block Component 
          -> CompTree
zipUp = zipUp' TopNode


-- like zipUp' but without reversing directions children must be reversed!
zipBack :: RoseTreeCxt Block Component -> CompTree
zipBack (In cxt (Block p d _ _ _ _) ls rs) = RoseTree newInfo newChildren
    where newMid = zipBack cxt
          newChildren = map flipBlockBack $ reverse ls ++ newMid:rs
          newSize = sum (map size newChildren)
          addRes True = sumP newChildren
          addRes False = sumS newChildren
          newInfo = Block p d newSize (addRes p) 0 0
zipBack (TopWith top) = RoseNode top


--flipping functions
flipBlock :: CompTree -> CompTree
flipBlock (RoseTree (Block p d s r v c) subs) =
    compTree p (spin d) s r v c (map flipBlock (reverse subs))
flipBlock (RoseNode (Resistor d r v c)) =
    RoseNode $ Resistor (spin d) r v c 
flipBlock x = x

flipBlockBack :: CompTree -> CompTree
flipBlockBack (RoseTree (Block p Backward s r v c) subs) =
    compTree p Backward s r v c (map flipBlockBack (reverse subs))
flipBlockBack x = x

-- Find the initial node and zip up to it (without reversing block directions)
zipToNull :: CompZipper -> Maybe CompZipper
zipToNull ((TopNode src, cxt)) = Just (zipBack cxt, TopWith src)
zipToNull (z@(RoseNode _, _)) = petalRight z >>= zipToNull
zipToNull (z@(RoseTree _ _, _)) = (petalDown z >>= zipToNull) `mplus` (petalRight z >>= zipToNull)


-- lift from a Zipper to a possible new net we can power
liftUp :: CompZipper -> Maybe CompNetz
liftUp (RoseNode (Source s), cxt@(In _ (Block p _ _ _ _ _) _ _)) = Just (zipUp' TopNode (not p, False) cxt, Source s)
liftUp _ = Nothing

liftPowerRestore :: CompZipper -> Maybe CompTree
liftPowerRestore x = liftUp x >>= (zipToNull . netzToZipper . powerNetz) >>= Just . fst

--add two trees
superImpose :: CompTree -> CompTree -> Maybe CompTree
superImpose (RoseTree b1 subs1) (RoseTree b2 subs2) =  do
        b <- b1 <?> b2
        rest <- sequence $ zipWith superImpose subs1 subs2
        return $ RoseTree b rest
superImpose (RoseNode a) (RoseNode b) = a <?> b >>= Just . RoseNode
superImpose _ _ = Nothing

-- POWER STUFF (CALCULATING PROPERTIES FOR A SPECIFIC TREE
powerNetz :: CompNetz -> CompNetz
powerNetz (tree, Source src) = (power src tree, Source src)
powerNetz n = n

-- FRONT END INTERFACE FUNCTIONS
calculate :: CompNetz -> Maybe CompNetz
calculate netz = case finalTree of
                    (Just t) -> Just (t, initialSrc)
                    Nothing -> Nothing
    where (initialTree, initialSrc) = powerNetz netz
          finalTree = foldWithMaybe superImpose initialTree
                        (mapMaybe liftPowerRestore (findAllSources netz))


--power tree with source
power :: Source -> CompTree -> CompTree
power (VoltageSource vol) (RoseTree (Block par dir num (Value res) _ _) rs) =
    distribute $ compTree par dir num (Value res) vol cur rs
    where cur = vol / res

power (CurrentSource cur) (RoseTree (Block par dir num (Value res) _ _) rs) =
    distribute $ compTree par dir num (Value res) vol cur rs
    where vol = res * cur

-- if we only have voltage sources in serial blocks then we're golden!
-- current sources only in parallel
distribute :: CompTree -> CompTree
distribute (RoseTree (Block True dir num (Value res) vol cur) rs) =
    compTree True dir num (Value res) vol cur (map infuse rs)
    where infuse (RoseTree (Block par' dir' num' (Value subRes) _ _) rs') =
            let newCur = res / subRes * cur
                newVol = vol
                -- the sort of inverse of the voltage divider rule
            in distribute $
                compTree par' dir' num' (Value subRes) newVol newCur rs'
          infuse (RoseTree (Block par' dir' num' Null _ _) rs') =
            distribute $ compTree par' dir' num' Null 0 cur rs'
          infuse (RoseTree (Block par' dir' num' Inf _ _) rs') =
            distribute $ compTree par' dir' num' Inf vol 0 rs'
          infuse (RoseNode (Resistor dir' (Value subRes) _ _)) =
            let newCur = res / fromIntegral subRes * cur
                newVol = vol
            in RoseNode (Resistor dir' (Value subRes) newVol newCur)
          infuse (RoseNode (Resistor dir' Null _ _)) =
            RoseNode (Resistor dir' Null 0 cur)
          infuse (RoseNode (Resistor dir' Inf _ _)) =
            RoseNode (Resistor dir' Inf vol 0)
          infuse b = b

distribute (RoseTree (Block False dir num (Value res) vol cur) rs) =
    compTree False dir num (Value res) vol cur (map infuse rs)
    where infuse (RoseTree (Block par' dir' num' (Value subRes) _ _) rs') =
            let newVol = subRes / res * vol
                newCur = cur
            in distribute $
                compTree par' dir' num' (Value subRes) newVol newCur rs'
          infuse (RoseTree (Block par' dir' num' Null _ _) rs') =
            distribute $ compTree par' dir' num' Null 0 cur rs'
          infuse (RoseTree (Block par' dir' num' Inf _ _) rs') =
            distribute $ compTree par' dir' num' Inf vol 0 rs'
          infuse (RoseNode (Resistor dir' (Value subRes) _ _)) =
            let newVol = fromIntegral subRes / res * vol
                newCur = cur
            in RoseNode (Resistor dir' (Value subRes) newVol newCur)
          infuse (RoseNode (Resistor dir' Null _ _)) =
            RoseNode (Resistor dir' Null 0 cur)
          infuse (RoseNode (Resistor dir' Inf _ _)) =
            RoseNode (Resistor dir' Inf vol 0)
          --catch TopNode
          infuse b = b

distribute (RoseTree (Block p' d' num Null 0 cur) rs) =
    compTree p' d' num Null 0 cur (map infuse rs)
    where infuse (RoseTree (Block p d n r _ _) rs') =
            distribute $ compTree p d n r 0 cur rs'
          infuse (RoseNode (Resistor d r _ _)) = RoseNode (Resistor d r 0 0)
          infuse b = b

distribute (RoseTree (Block p' d' num' Inf vol 0) rs) =
    compTree p' d' num' Inf vol 0 (map infuse rs)
    where infuse (RoseTree (Block p d n r _ _) rs') =
            distribute $ compTree p d n r vol 0 rs'
          infuse (RoseNode (Resistor d r _ _)) = RoseNode (Resistor d r 0 0)
          infuse b = b

--distribute b = b


--
--
--GENERATING CIRCUITS--
--
data SplitState = SplitState { maxSize :: Int, maxRes :: Int, currGen :: StdGen }

type Split a = MaybeState SplitState a

--random num generating functions. see pickWith
pickWith :: (StdGen -> (b, StdGen)) -> Split b
pickWith f = do
    SplitState num res g <- get
    let (num1, g1) = f g
    put $ SplitState num res g1
    return num1

--double from the inner 60% of the input range
middle :: Double -> Double -> StdGen -> (Double, StdGen)
middle high low = randomR (low+offset, high-offset)
    where offset = 0.2* (high-low)

--int from the inner 60% of the input range
middleI :: Int -> Int -> StdGen -> (Int, StdGen)
middleI high low = randomR (low+offset, high-offset)
    where offset = round $ 0.2 * fromIntegral (high-low)

--number around the square root of the input
sqrRoot :: Int -> StdGen -> (Int, StdGen)
sqrRoot high' = randomR (low,high)
    where low = max (round $ sqrt (fromIntegral high' :: Double)) 1
          high = min (round $ sqrt (fromIntegral high' :: Double) + 2) (high' -1)

--"push" the chances of a bool being the opposite of the input
opposite :: Bool -> StdGen -> (Bool, StdGen)
opposite parent g = (withParent parent . toWeighted $ randomInt, g1)
        where (randomInt, g1) = randomR (0, 9) g :: (Int, StdGen)
              toWeighted b = b >= 9
              withParent parent b = if parent then b
                                              else not b

--search starting at the input for a whole number resistor
--this comes from the equation for parallel resistor total resistance
findWholeNumberRes :: Int -> StdGen -> ((Int,Int),StdGen)
findWholeNumberRes maxVal g = checkPossibilities (round $ fromIntegral maxVal * 1.3)
    where checkPossibilities step = if mod (maxVal * step) (step - maxVal) == 0
            then ((step, round $ fromIntegral (maxVal*step) / fromIntegral (step - maxVal)),g)
            else checkPossibilities (step+1)

findDoubleRes :: Double -> StdGen -> ((Double, Double), StdGen)
findDoubleRes maxVal g = ((newRes, newHigh), g1)
    where (newRes, g1) = randomR (maxVal, 10*maxVal) g
          newHigh = maxVal*newRes / (newRes - maxVal)

--guess a whole number instead
guessRes :: Int -> StdGen -> ((Int, Int), StdGen)
guessRes maxGuess g = let (guess,g1) = randomR (round $ fromIntegral maxGuess*lowC,maxGuess*highC) g
                 in if mod (maxGuess*guess) (guess-maxGuess) == 0 && guess /= maxGuess*2
                        then (findRes guess maxGuess,g1)
                        else guessRes maxGuess g1
    where findRes guess maxGuess = (guess, (round $ fromIntegral (maxGuess*guess) /
                fromIntegral (guess-maxGuess)))
          lowC = 1.3 :: Double
          highC = 3 :: Int


--splitBlock :: CompTree -> State StdGen CompTree
splitBlock :: CompTree -> StdGen -> CompTree


--we're splitting a serial block
--we pick a size less than this block's size,
--split it at this size and then pick a random res
--and then calculate the resulting voltage and current
--we just pick the opposite of the parent block for the orientation
--this results in a more orderly network in the end
--TODO: totality: add splitting inf and null blocks, not really important though
splitBlock (RoseTree (Block False d curMaxSize (Value res) _ _) []) initGen =
    RoseTree (Block False d curMaxSize (Value res) 0 0)
             (unfoldr (runMaybeState genNewBlock) initState)
    where initState = SplitState curMaxSize (round res) initGen
          genNewBlock = do
            SplitState highNum highRes thisGen <- get
            case compare highNum 1 of
                LT -> end
                EQ -> do
                    put $ SplitState 0 0 thisGen
                    return $ RoseNode (Resistor d (Value highRes) 0 0)
                GT -> do
                    newSize' <- pickWith $ sqrRoot curMaxSize
                    let newSize = min newSize' highNum
                    subRes' <- pickWith $ middleI 0 highRes
                    let subRes = if newSize == highNum
                                    then highRes
                                    else subRes'
                    let isPar = True
                        newBlock = case newSize of
                                    1 -> RoseNode (Resistor d (Value subRes) 0 0)
                                    _ -> compTree isPar d newSize (Value (fromIntegral subRes)) 0 0 []
                        newState = SplitState (highNum - newSize) (highRes - subRes)

                    (g1, g2) <- gets (split . currGen)
                    putStateAndReturn (newState g1) (splitBlock newBlock g2)

-- we're splitting a parallel block
-- similar to the serial block but we pick a random current here
-- probably can be done with resistance as well
splitBlock (RoseTree (Block True d curMaxSize (Value res) _ _) []) initGen =
    RoseTree (Block True d curMaxSize (Value res) 0 0)
             (unfoldr (runMaybeState genNewBlock) initState)
    where initState = SplitState curMaxSize (round res) initGen
          genNewBlock = do
            SplitState highNum highRes thisGen <- get
            case compare highNum 1 of
                LT -> end
                EQ -> do
                    put $ SplitState 0 0 thisGen
                    return $ RoseNode (Resistor d (Value highRes) 0 0)
                GT -> do
                    newSize' <- pickWith $ sqrRoot curMaxSize
                    let newSize = if newSize' > highNum
                                    then highNum
                                    else newSize'
                    (subRes', newHighRes) <- pickWith $ findWholeNumberRes highRes
                    let subRes = if newSize == highNum 
                                    then highRes
                                    else subRes' --FROMwith whole numer(fromIntegral subRes')
                    let isPar = False
                        newBlock = case newSize of 
                                    1 -> RoseNode (Resistor d (Value subRes) 0 0)
                                    _ -> compTree isPar d newSize (Value (fromIntegral subRes)) 0 0 []
                        newState = SplitState (highNum - newSize) newHighRes--FROM with whole numer(fromIntegral newHighRes)
                    (g1, g2) <- gets (split . currGen)
                    putStateAndReturn (newState g1) (splitBlock newBlock g2)

splitBlock r@(RoseNode _) _ = r
splitBlock r@(TopNode _) _ = r



-- MAYBESTATET MONAD STUFF
newtype MaybeStateT s m a =
    MaybeStateT { runMaybeStateT :: s -> m (Maybe (a,s)) }

instance MonadTrans (MaybeStateT s) where
    lift m = MaybeStateT $ \s -> do
        a <- m
        return $ Just (a,s)

instance (Monad m) => Functor (MaybeStateT s m) where
    fmap f m = MaybeStateT $ \s -> do
        inner <- runMaybeStateT m s
        case inner of
            Nothing -> return Nothing
            Just ~(x, s') -> return $ Just (f x, s')

instance (Monad m) => Applicative (MaybeStateT s m) where
  f <*> a = MaybeStateT $ \s0 -> do
                            inner <- runMaybeStateT f s0
                            case inner of
                              Nothing -> return Nothing
                              Just (f1, s1) -> runMaybeStateT (f1 <$> a) s1
  pure a = MaybeStateT $ \s -> return . Just $ (a,s)

instance (Monad m) => Monad (MaybeStateT s m) where
    m >>= f = MaybeStateT (\s0 -> do
                inner <- runMaybeStateT m s0
                case inner of
                    Nothing -> return Nothing
                    Just (val1, s1) -> runMaybeStateT (f val1) s1)
    return = pure

instance (Monad m) => MonadState s (MaybeStateT s m) where
    get = MaybeStateT $ \s -> return . Just $ (s,s)
    put s = MaybeStateT $ \_ -> return . Just $ ((),s)

end :: (Monad m) => MaybeStateT s m a
end = MaybeStateT (\_ -> return Nothing)

putStateAndReturn :: (Monad m) => s -> a -> MaybeStateT s m a
putStateAndReturn newState acc =
    MaybeStateT $ \_ -> return . Just $ (acc, newState)

--Simple Monad
type MaybeState s a = MaybeStateT s Identity a

runMaybeState :: MaybeState s a -> s -> Maybe (a, s)
runMaybeState m = runIdentity . runMaybeStateT m
