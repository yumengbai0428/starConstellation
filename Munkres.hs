
-- | The Munkres version of the Hungarian Method for weighted minimal 
-- bipartite matching. 
-- The implementation is based on Robert A. Pilgrim's notes, 
-- <http://216.249.163.93/bob.pilgrim/445/munkres.html>
-- (mirror: <http://www.public.iastate.edu/~ddoty/HungarianAlgorithm.html>).
{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleContexts #-}
module Munkres
  ( 
    -- hungarianMethod 
    hungarianMethodInt
  , hungarianMethodFloat
  , hungarianMethodDouble
  , hungarianMethodBoxed
  
#ifdef MUNKRES_DEBUG  
  , tesztek, teszt1, teszt2, teszt3, teszt4
  , bruteforce
  , randomArray
  , doATest, doFloatTest
  , doManyTests, doManyFloatTests
  , main
#endif

  ) where

import Prelude hiding (flip)
  
import Control.Monad
import Control.Monad.ST

import Data.List hiding (insert)

import Data.STRef
import Data.Array.ST

import Data.Array.IArray ()
import Data.Array.MArray
import Data.Array.Unboxed

#ifdef MUNKRES_DEBUG  
import Data.Ord (comparing)
import Debug.Trace
import System.Random
#endif

#if MIN_VERSION_base(4,9,0)
patternFail :: Monad m => String -> m a
patternFail msg = errorWithoutStackTrace msg
#else
patternFail :: Monad m => String -> m a
patternFail msg = error msg
#endif
-------------------------------------------------------

swap :: (Int,Int) -> (Int,Int)
swap (x,y) = (y,x)

{-
complementSort :: Int -> [Int] -> [Int]
complementSort n xs = complement n (sort xs)
-}

-- assumes that the input is sorted
complement :: Int -> [Int] -> [Int]
complement n list = worker 1 list where
  worker k xxs@(x:xs) = if k>n 
    then []
    else case compare k x of
      EQ -> worker (k+1) xs
      LT -> k : worker (k+1) xxs
      GT -> worker k xs
  worker k [] = [k..n]

{-
merge :: [Int] -> [Int] -> [Int]
merge xxs@(x:xs) yys@(y:ys) = if x <= y 
  then x : merge xs yys
  else y : merge xxs ys 
merge xs [] = xs
merge [] ys = ys
-}

-- assumes that the inputs are sorted sets 
mergeUnion :: [Int] -> [Int] -> [Int]
mergeUnion xxs@(x:xs) yys@(y:ys) = case compare x y of
  LT -> x : mergeUnion xs yys
  EQ -> x : mergeUnion xs  ys
  GT -> y : mergeUnion xxs ys 
mergeUnion xs [] = xs
mergeUnion [] ys = ys

insert :: Int -> [Int] -> [Int]
insert y xxs@(x:xs) = case compare y x of
  LT -> y : xxs 
  EQ -> xxs
  GT -> x : insert y xs 
insert y [] = [y]

remove :: Int -> [Int] -> [Int]
remove y xxs@(x:xs) = case compare y x of
  LT -> xxs
  EQ -> xs
  GT -> x : remove y xs 
remove _ [] = []

{-# SPECIALIZE firstJust :: [ ST s (Maybe (Int,Int)) ] -> ST s (Maybe (Int,Int)) #-}
firstJust :: Monad m => [ m (Maybe a) ] -> m (Maybe a)
firstJust (a:as) = do
  x <- a
  case x of
    Just _ -> return x
    Nothing -> firstJust as
firstJust [] = return Nothing

{-# SPECIALISE alternate :: [Int] -> ([Int],[Int]) #-}
alternate :: [a] -> ([a],[a])
alternate list = flip list [] [] where
  flip (x:xs) ys zs = flop xs (x:ys) zs
  flip [] ys zs = (reverse ys,reverse zs)
  flop (x:xs) ys zs = flip xs ys (x:zs)
  flop [] ys zs = (reverse ys,reverse zs)

-------------------------------------------------------

-- polymorphicity problem workaround experiment...

thawST :: (IArray a e, MArray (STArray s) e (ST s)) => a (Int,Int) e -> ST s (STArray s (Int,Int) e) 
thawST = thaw

thawSTU :: (IArray UArray e, MArray (STUArray s) e (ST s)) => UArray (Int,Int) e -> ST s (STUArray s (Int,Int) e) 
thawSTU = thaw

newSTArray_ :: MArray (STArray s) e (ST s) => ((Int,Int),(Int,Int)) -> ST s (STArray s (Int,Int) e)
newSTArray_ = newArray_

newSTUArray_ :: MArray (STUArray s) e (ST s) => ((Int,Int),(Int,Int)) -> ST s (STUArray s (Int,Int) e)
newSTUArray_ = newArray_

-------------------------------------------------------

{- SPECIALISE hungarianMethod :: UArray (Int,Int) Int    -> ([(Int,Int)],Int   ) -}
{- SPECIALISE hungarianMethod :: UArray (Int,Int) Float  -> ([(Int,Int)],Float ) -}
{- SPECIALISE hungarianMethod :: UArray (Int,Int) Double -> ([(Int,Int)],Double) -}

-- | Needs a rectangular array of /nonnegative/ weights, which
-- encode the weights on the edges of a (complete) bipartitate graph.
-- The indexing should start from @(1,1)@.
-- Returns a minimal matching, and the cost of it.
-- 
-- Unfortunately, GHC is opposing hard the polymorphicity of this function. I think
-- the main reasons for that is that the there is no @Unboxed@ type class, and
-- thus the contexts @IArray UArray e@ and @MArray (STUArray s) e (ST s)@ do not
-- know about each other. (And I have problems with the @forall s@ part, too).

hungarianMethodInt :: UArray (Int,Int) Int -> ([(Int,Int)],Int) 
hungarianMethodInt input = runST $ do
  let ((1,1),(n,m)) = bounds input
  star <- if m >= n 
    then do 
      ar <- thawSTU input
      hungarianMethodShared ar
    else do
      ar <- newSTUArray_ ((1,1),(m,n))  
      forM_ [ (i,j) | i<-[1..n] , j<-[1..m] ] $ \(i,j) -> do
        writeArray ar (j,i) $ input ! (i,j) 
      star' <- hungarianMethodShared ar
      return (map swap star') 
  let costs = [ input ! ij | ij <- star ]
  return (star, sum costs)

hungarianMethodFloat :: UArray (Int,Int) Float -> ([(Int,Int)],Float) 
hungarianMethodFloat input = runST $ do
  let ((1,1),(n,m)) = bounds input
  star <- if m >= n 
    then do 
      ar <- thawSTU input
      hungarianMethodShared ar
    else do
      ar <- newSTUArray_ ((1,1),(m,n)) 
      forM_ [ (i,j) | i<-[1..n] , j<-[1..m] ] $ \(i,j) -> do
        writeArray ar (j,i) $ input ! (i,j) 
      star' <- hungarianMethodShared ar
      return (map swap star') 
  let costs = [ input ! ij | ij <- star ]
  return (star, sum costs)

hungarianMethodDouble :: UArray (Int,Int) Double -> ([(Int,Int)],Double) 
hungarianMethodDouble input = runST $ do
  let ((1,1),(n,m)) = bounds input
  star <- if m >= n 
    then do 
      ar <- thawSTU input
      hungarianMethodShared ar
    else do
      ar <- newSTUArray_ ((1,1),(m,n)) 
      forM_ [ (i,j) | i<-[1..n] , j<-[1..m] ] $ \(i,j) -> do
        writeArray ar (j,i) $ input ! (i,j) 
      star' <- hungarianMethodShared ar
      return (map swap star') 
  let costs = [ input ! ij | ij <- star ]
  return (star, sum costs)

-- | The same as 'hungarianMethod<Type>', but uses boxed values (thus works with
-- any data type which an instance of 'Real'). 
-- The usage of one the unboxed versions is recommended where possible, 
-- for performance reasons.
hungarianMethodBoxed :: (Real e, IArray a e) => a (Int,Int) e -> ([(Int,Int)],e)
hungarianMethodBoxed input = runST $ do
  let ((1,1),(n,m)) = bounds input
  star <- if m >= n 
    then do 
      ar <- thawST input -- :: ST s (STArray s (Int,Int) e)
      hungarianMethodShared ar
    else do
      ar <- newSTArray_ ((1,1),(m,n)) -- :: ST s (STArray s (Int,Int) e)
      forM_ [ (j,i) | j<-[1..m] , i<-[1..n] ] $ \(j,i) ->
        writeArray ar (j,i) $ input ! (i,j) 
      star' <- hungarianMethodShared ar
      return (map swap star') 
  let costs = [ input ! ij | ij <- star ]
  return (star, sum costs)


{-# SPECIALISE hungarianMethodShared :: STUArray s (Int,Int) Int    -> ST s [(Int,Int)] #-}
{-# SPECIALISE hungarianMethodShared :: STUArray s (Int,Int) Float  -> ST s [(Int,Int)] #-}
{-# SPECIALISE hungarianMethodShared :: STUArray s (Int,Int) Double -> ST s [(Int,Int)] #-}

hungarianMethodShared :: (Real e, MArray a e (ST s)) => a (Int,Int) e -> ST s [(Int,Int)]
hungarianMethodShared ar = do
  starred <- newSTRef []
  primed  <- newSTRef []
  coveredRows <- newSTRef []
  coveredCols <- newSTRef []
  bounds <- getBounds ar
  case bounds of
    ((1,1), nm) -> munkers ar nm starred primed coveredRows coveredCols
    _ -> patternFail "Invalid bounds"

-- the meat comes here...

{-# SPECIALISE munkers :: 
     STUArray s (Int,Int) Int -> (Int,Int) 
  -> STRef s [(Int,Int)] -> STRef s [(Int,Int)] 
  -> STRef s [Int] -> STRef s [Int]
  -> ST s [(Int,Int)] #-}
  
{-# SPECIALISE munkers :: 
     STUArray s (Int,Int) Float -> (Int,Int)  
  -> STRef s [(Int,Int)] -> STRef s [(Int,Int)] 
  -> STRef s [Int] -> STRef s [Int]
  -> ST s [(Int,Int)] #-}
  
{-# SPECIALISE munkers :: 
     STUArray s (Int,Int) Double -> (Int,Int)  
  -> STRef s [(Int,Int)] -> STRef s [(Int,Int)] 
  -> STRef s [Int] -> STRef s [Int]
  -> ST s [(Int,Int)] #-}
  
munkers :: (Real e, MArray a e (ST s)) 
  => a (Int,Int) e -> (Int,Int) 
  -> STRef s [(Int,Int)] -> STRef s [(Int,Int)] 
  -> STRef s [Int] -> STRef s [Int]
  -> ST s [(Int,Int)]   

munkers ar (n,m) starred primed coveredRows coveredCols = (step1 >> step2 >> step3) where

  kk = min n m

  step3 = do
    --printArray "step3"
    colsC <- readSTRef coveredCols
    star <- readSTRef starred
    let colsC' = mergeUnion colsC (sort $ map snd star) -- nub $ colsC ++ (map snd star)
    if length colsC' == kk
      then return star
      else do
        writeSTRef coveredCols colsC'
        step4
        
  step4 = do
    --printArray "step4"
    --printPrimStar "step4"
    rowsC <- readSTRef coveredRows
    colsC <- readSTRef coveredCols
    let rowsNC = complement n rowsC
        colsNC = complement m colsC
    star <- readSTRef starred   
    let f ij = do
          x <- readArray ar ij 
          if x==0 then return (Just ij) else return Nothing 
    mp <- firstJust [ f (i,j) | i<-rowsNC, j<-colsNC ] 
    --print mp
    case mp of
      Nothing -> do
        es <- forM [ (i,j) | i<-rowsNC, j<-colsNC ] $ \ij -> readArray ar ij
        step6 (minimum es)   
      Just ij@(i,_) -> do
        modifySTRef primed (ij:) 
        case find (\(p,_) -> p==i) star of
          Nothing -> step5 ij
          Just (_,q) -> do
            modifySTRef coveredRows (insert i) 
            modifySTRef coveredCols (remove q) 
            step4
{-        
        case filter (\(p,_) -> p==i) star of
          [] -> step5 ij
          [(p,q)] -> do
            modifySTRef coveredRows (insert i) 
            modifySTRef coveredCols (remove q) 
            step4
          _ -> error "Munkres/step4: should not happen"
-}

  step5 pq = do
    --printArray "step5"
    --printPrimStar "step5"
    star <- readSTRef starred
    prim <- readSTRef primed
    alt <- step5a star prim pq [pq]
    let (ps,ss) = alternate alt
    writeSTRef starred $ (star \\ ss) ++ ps
    writeSTRef primed []
    writeSTRef coveredRows []
    writeSTRef coveredCols []
    step3 
    
  step5a :: [(Int,Int)] -> [(Int,Int)] -> (Int,Int) -> [(Int,Int)] -> ST s [(Int,Int)]
  step5a star prim (_,q) xs = 
    case findStarred q of
      Just (i,_) -> do
        let (_,j) = findPrimed i
        step5a star prim (i,j) ((i,j):(i,q):xs)
      Nothing -> return xs
    where
      findStarred j =      find (\(_,c) -> (c==j)) star
      findPrimed  i = case find (\(r,_) -> (r==i)) prim of
        Just x  -> x
        Nothing -> error $ "Munkres/findPrimed: should not happen (" ++ show prim ++ " " ++ show i ++ ")"
        
  step2 = 
    do 
      --printArray "step2"
      s <- foldM worker [] [ (i,j) | i<-[1..n], j<-[1..m] ] 
      writeSTRef starred s
    where
      worker star ij@(i,j) = do
        x <- readArray ar ij
        if x==0 
          then case filter (\(a,b) -> (a==i) || (b==j)) star of
            [] -> return (ij : star)
            _ -> return star
          else return star

  step6 c = do
    --printArray "step6"
    --printPrimStar "step6"
    rowsC <- readSTRef coveredRows
    colsC <- readSTRef coveredCols
    let rowsNC = complement n rowsC
        colsNC = complement m colsC
    forM rowsNC $ \i -> 
      forM colsNC $ \j -> do
        x <- readArray ar (i,j)
        writeArray ar (i,j) (x-c)
    forM rowsC $ \i -> 
      forM colsC $ \j -> do
        x <- readArray ar (i,j)
        writeArray ar (i,j) (x+c)
    step4
        
  step1 = mapM_ subRow [1..n]
  subRow i = do
    row <- forM [1..m] $ \j -> readArray ar (i,j)
    let y = minimum row
    forM [1..m] $ \j -> do
      let ij = (i,j)
      x <- readArray ar ij
      writeArray ar ij (x-y)
      
{- 
  -- debugging
  
  printArray s = do
    putStrLn ""
    x <- freeze ar :: IO (UArray (Int,Int) Int)
    print (s,x)
    
  printPrimStar s = do
    star <- readSTRef starred
    prim <- readSTRef primed
    crows <- readSTRef coveredRows
    ccols <- readSTRef coveredCols
    putStrLn s
    print ("starred",star)
    print ("primed",prim)
    print ("cov. rows",crows)
    print ("cov. cols",ccols)
-}

-------------------------------------------------------

#ifdef MUNKRES_DEBUG  

debug x y = trace (show x) y

-- brute-force algorithm for sanity checking

bruteforce :: UArray (Int,Int) Int -> ([(Int,Int)],Int)
bruteforce input = {- debug all $ -} minimumBy (comparing snd) allWithCosts where
  ((1,1),(n,m)) = bounds input 
  k = min n m
  g = if n<m then id else swap
  lookup = (input!)  
  all = f [1..min n m] [1..max n m] 
  f [] _ = [[]]
  f _ [] = [[]]
  f (i:is) js = concat [ map ((i,j):) (f is (remove j js)) | j<-js ]
  withCost ijs' = let ijs = map g ijs' in ( ijs , sum (map lookup ijs) )
  allWithCosts = map withCost all
 
-- random array
-- why on earth is 'randomR' using the opposite convention of what 'mapAccumL' uses ?!?!?!? 

randomR' g1 iv = let (x,g2) = randomR iv g1 in (g2,x)

randomArray :: RandomGen g => Int -> Int -> g -> (UArray (Int,Int) Int , g) 
randomArray maxsize maxelem rnd0 = (ar,rnd3) where
  (n,rnd1) = randomR (1,maxsize) rnd0
  (m,rnd2) = randomR (1,maxsize) rnd1
  (rnd3,es) = mapAccumL randomR' rnd2 $ replicate (n*m) (0::Int,maxelem)
  ar = listArray ((1,1),(n,m)) es 

-- correctness testing
 
doATest maxsize maxelem rnd0 _ = do
  let (ar,rnd1) = randomArray maxsize maxelem rnd0
      (xs,c) = hungarianMethodInt ar
      sol1 = (sortBy (comparing fst) xs, c)
      sol2 = bruteforce ar
  when (snd sol1 /= snd sol2) $ do
    print ar
    putStrLn $ show (snd $ bounds ar) ++ ": " ++ show (snd sol1) ++ " " ++ show (snd sol2)
    putStrLn $ "hun -> " ++ show (fst sol1) ++ "\nbrt -> " ++ show (fst sol2)
  return rnd1

-- int vs float testing (mainly because of the copypasted code)

doFloatTest maxsize maxelem rnd0 _ = do
  let (ar,rnd1) = randomArray maxsize maxelem rnd0
      sol1 = hungarianMethodInt ar
      sol2 = hungarianMethodFloat  (amap fromIntegral ar) 
      sol3 = hungarianMethodDouble (amap fromIntegral ar) 
  print (snd sol1, snd sol2, snd sol3)
  return rnd1

-- do lots of tests
    
doManyTests n maxsize maxelem = getStdGen >>= \rnd ->
  foldM_ (doATest maxsize maxelem) rnd [1..n] 

doManyFloatTests n maxsize maxelem = getStdGen >>= \rnd ->
  foldM_ (doFloatTest maxsize maxelem) rnd [1..n] 

main = do
  putStrLn "a"
  doManyTests 50 10 10
  putStrLn "b"
  doManyTests 50 10 50
  putStrLn "c"
  doManyTests 50 10 100
  putStrLn "d"
  doManyTests 100 10 10
  putStrLn "e"
  doManyTests 100 10 50
  putStrLn "f"
  doManyTests 100 10 100
    
#endif
                 
-------------------------------------------------------
               
-- some test cases                 

#ifdef MUNKRES_DEBUG  
                
tesztek = [ teszt1, teszt2, teszt3, teszt4 ]                 
                 
teszt1 :: UArray (Int,Int) Int
teszt1 = listArray ((1,1),(3,3)) $ concat $ transpose $
  [ [ 1,2,3 ]
  , [ 2,4,6 ]
  , [ 3,6,9 ]
  ]            

teszt2 :: UArray (Int,Int) Int
teszt2 = listArray ((1,1),(4,4)) $ concat $ transpose $
  [ [ 14,5,8,7 ]
  , [ 2,12,6,5 ]
  , [ 7,8,3,9  ]
  , [ 2,4,6,10 ]
  ]            
  
teszt3 :: UArray (Int,Int) Int
teszt3 = listArray ((1,1),(5,5)) $ concat $ transpose
  [ [4,5,3,2,3]
  , [3,2,4,3,4]
  , [3,3,4,4,3]
  , [2,4,3,2,4]
  , [2,1,3,4,3]
  ]

teszt4 :: UArray (Int,Int) Int
teszt4 = listArray ((1,1),(6,6)) $ concat $ transpose
  [ [ 3,4,5,6,2,1 ]
  , [ 3,0,1,2,3,4 ]
  , [ 7,6,0,2,1,1 ]
  , [ 4,4,5,0,1,2 ]
  , [ 0,1,0,1,0,0 ]
  , [ 0,3,2,2,2,0 ]
  ]

#endif
  
-------------------------------------------------------
