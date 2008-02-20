{-# LANGUAGE Rank2Types, ScopedTypeVariables, PatternSignatures,
             MultiParamTypeClasses, DeriveDataTypeable, 
             GeneralizedNewtypeDeriving, TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-name-shadowing #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Param.FSVec
-- Copyright   :  (c) 2008 Alfonso Acosta, Oleg Kiselyov, Wolfgang Jeltsch
--                    and KTH's SAM group 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  alfonso.acosta@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- 'FSVec': Fixed sized vectors. Vectors with numerically parameterized size.
--
----------------------------------------------------------------------------
module Data.Param.FSVec 
  (FSVec, empty, (+>), singleton, vectorCPS, vectorTH,
#if __GLASGOW_HASKELL__ >= 609
   v,
#endif
   unsafeVector, readFSVec, length, genericLength,
   lengthT, fromVector, null, (!), replace, head, last,
   init, tail, take, drop, select, group, (<+), (++),
   map, zipWith, foldl, foldr, zip, unzip, shiftl, shiftr,
   rotl, rotr, concat, reverse, iterate, generate, copy 
  ) where

import Data.TypeLevel.Num hiding ((-),(+),(*),(>),(<),(>=),(<=),(==))
import Data.TypeLevel.Num.Aliases.TH (dec2TypeLevel)

import Data.Generics (Data, Typeable)
import qualified Prelude as P
import Prelude hiding (
              null, length, head, tail, last, init, take, drop, 
	      (++), map, foldl, foldr, 
	      zipWith, zip, unzip, 
	      concat, reverse, iterate)
import qualified Data.Foldable  as DF (Foldable, foldr)
import qualified Data.Traversable as DT (Traversable(traverse)) 
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
#if __GLASGOW_HASKELL__ >= 609
import Language.Haskell.TH.Quote
#endif


-- | Fixed-Sized Vector data type, indexed with type-level naturals, the 
--   first index for all vectors is 0
newtype Nat s => FSVec s a = FSVec {unFSVec :: [a]}
 deriving (Eq, Typeable, Data)

instance Show a => Show (FSVec s a) where
 showsPrec _  = showV.unFSVec
  where showV []       = showString "<>"
        showV (x:xs)   = showChar '<' . shows x . showl xs
                        where showl []     = showChar '>'
                              showl (x:xs) = showChar ',' . shows x .
                                             showl xs

-------------------------
-- Constructing functions
-------------------------

empty :: FSVec D0 a
empty = FSVec []

-- | Cons operator, note it's not a constructor
(+>) :: (Nat s, Pos s', Succ s s') => a -> FSVec s a -> FSVec s' a
x +> (FSVec xs) = FSVec (x:xs)

infixr 5 +>


-- | A FSVec with a single element
singleton :: a -> FSVec D1 a
singleton x = x +> empty


-- | Build a vector from a list (CPS style)
vectorCPS :: [a] -> (forall s . Nat s => FSVec s a -> w) -> w
vectorCPS xs = unsafeVectorCPS (P.length xs) xs

-- | Build a vector from a list (using Template Haskell)
vectorTH :: Lift a => [a] -> ExpQ
vectorTH xs = (vectorCPS xs) lift

#if __GLASGOW_HASKELL__ >= 609
-- | Vector quasiquoter
v :: QuasiQuoter
v = QuasiQuoter parseFSVecExp parseFSVecPat

-- Build a vector using quasiquotation
-- Not possible in the general case! It is feasible, though, when only 
-- allowing monomorphic vectors. For example, in the case of Ints:
-- parseFSVecExp :: String -> ExpQ
-- parseFSVecExp str = (readFSVec str) (lift :: Nat s => FSVec s Int -> ExpQ)
parseFSVecExp :: String -> ExpQ
parseFSVecExp str = undefined -- (readFSVec str) lift  

-- Pattern match a vector using quasiquotation
parseFSVecPat :: String -> PatQ
parseFSVecPat = error "Data.Param.FSVec: quasiquoting paterns not supported"

-- __GLASGOW_HASKELL__
#endif 

-- | Build a vector from a list (unsafe version: The static/dynamic size of 
--   the list is checked at runtime)
unsafeVector :: Nat s => s -> [a] -> FSVec s a
unsafeVector l xs
 | toNum l /= P.length xs = 
      error "unsafeVector: dynamic/static length mismatch"
 | otherwise = FSVec xs

-- | Read a vector. As it happens with vectorCPS, this is the best we can 
--   achieve
readFSVec :: Read a => String -> (forall s . Nat s => FSVec s a -> w) -> w
readFSVec str = unsafeVectorCPS l xs
 where 
  (xs, l) =  readFSVecList str
  -- modified version of Prelude.read which accepts < > instead
  -- of [ ] to read lists and also provides the list length
  readFSVecList             :: (Read a) => String -> ([a], Int)
  readFSVecList s =  case [(x,n) | (x,n,t) <- readList' s, ("","") <- lex t] of
                            [x] -> x
                            []  -> error "FSVec.readFSVec: no parse"
                            _   -> error "FSVec.readFSVec: ambiguous parse"
   where
    readList' :: Read a => String -> [([a], Int, String)]
    readList'         = readParen' False (\r -> [pr | ("<",s)  <- lex r,
                                                      pr       <- readl s])
    readl  s = [([],0,t)   | (">",t)  <- lex s] P.++
                                          [(x:xs,1+n,u) | (x,t)    <- reads s,
                                                      (xs,n,u)   <- readl' t]
    readl' s = [([],0,t)   | (">",t)  <- lex s] P.++
                                          [(x:xs,1+n,v) | (",",t)  <- lex s,
                                                      (x,u)    <- reads t,
                                                      (xs,n,v)   <- readl' u]
    readParen' b g  = if b then mandatory else optional
                      where optional r  = g r P.++ mandatory r
                            mandatory r = [(x,n,u) | ("(",s) <- lex r,
                                                   (x,n,t)   <- optional s,
                                                 (")",u) <- lex t    ]
----------------------
-- Observing functions
----------------------

-- | value-level length of a vector 
length :: forall s a . Nat s => FSVec s a -> Int
length _ =  toInt (undefined :: s)

-- | generic value-level length of a vector 
genericLength :: forall s a n . (Nat s, Num n) => FSVec s a -> n
genericLength _ =  toNum (undefined :: s)

-- | type-level version of length
lengthT :: Nat s => FSVec s a -> s
lengthT = undefined

-- | Transform Vector to a list
fromVector ::  Nat s => FSVec s a -> [a]
fromVector (FSVec xs) = xs  

-- | Check if a Vector is empty
null :: FSVec D0 a -> Bool
null _ = True
-- Note: This definition checks the length at _runtime_, we don't want it
--       null (FSVec []) = True
--       null _          = False

-- | Access an element of a vector
(!) :: (Pos s, Nat i, i :<: s) => FSVec s a -> i -> a
(FSVec xs) ! i = xs !! (toInt i)

-------------------------
-- Transforming functions
-------------------------

-- | Replace an element of a vector
replace :: (Nat s, Nat i) => FSVec s a -> i -> a -> FSVec s a
-- alternative, more restrictive type 
-- replace :: (Pos s, Nat i, n :<: s) => FSVec s a -> i -> a -> FSVec s a
replace (FSVec xs) i y = FSVec $ replace' xs (toInt i) y
 where replace' []     _ _ = []
       replace' (_:xs) 0 y = (y:xs)
       replace' (x:xs) n y = x : (replace' xs (n - 1) y)

-- | Take the first element of a vector
head :: Pos s => FSVec s a -> a
head = P.head . unFSVec   

-- | Take the last element of a vector
last :: Pos s => FSVec s a -> a 
last = P.last . unFSVec   

-- | Return all but the first element of a vector
tail :: (Pos s, Succ s' s) => FSVec s a -> FSVec s' a
tail = liftV P.tail  
    
-- | Return all but the last element of a vector
init :: (Pos s, Succ s' s) => FSVec s a -> FSVec s' a
init = liftV P.init  
    
-- | Take the first i elements of a vector
take :: (Nat i, Nat s, Min s i s') => i -> FSVec s a -> FSVec s' a
take i = liftV $ P.take (toInt i) 

-- | Drop the first i elements of a vector
drop :: (Nat i, Nat s, Min s i sm, Sub s sm s') => i -> FSVec s a -> FSVec s' a
drop i = liftV $ P.drop (toInt i) 


-- | The function 'select' selects elements in the vector. The first argument
-- gives the initial element, starting from zero, the second argument gives the
-- stepsize between elements and the last argument gives the number of 
-- elements.
select :: (Nat f, Nat s, Nat n, f :<: i, {- f + s * n <= i -}
           Mul s n smn, Add f smn fasmn, fasmn :<=: i) => 
  f -> s -> n -> FSVec i a -> FSVec n a
select f s n = liftV (select' f' s' n')
  where (f', s', n') = (toInt f, toInt s, toInt n)
        select' f s n = ((selectFirst0 s n).(P.drop f)) 
        -- list version of select assuming 0 is the index for the first element
        selectFirst0 :: Int -> Int -> [a] -> [a]
        selectFirst0 s n l@(x:_) 
          | n > 0 = x : selectFirst0 s (n - 1) (P.drop s l)  
          | otherwise = []
        selectFirst0 _ 0 [] = []

-- | break a vector into subvectors of size n.
group :: (Pos n, Nat s, Div s n s') => 
         n -> FSVec s a -> FSVec s' (FSVec n a)
group n  = liftV (group' (toInt n)) 
   where group' :: Int -> [a] -> [FSVec s a]
         group' n xs = case splitAtM n xs of
                         Nothing -> []
                         Just (ls, rs) -> FSVec ls : group' n rs
 
-- | add an element at the end of a vector. (Inverse of '(+>)')         
(<+) :: (Nat s, Pos s', Succ s s') => FSVec s a -> a -> FSVec s' a
-- This should work, but it doesn't because
--      "Could not deduce (Data.TypeLevel.Num.Ops.Add' s D1 s')
--    from the context (Nat s, Pos s', Succ s s')"
-- xs <+ x = xs Data.Param.FSVec.++ (singleton x)
(<+) (FSVec xs) x = FSVec (xs P.++ [x]) 

-- | Concatenate two vectors
(++) :: (Nat s1, Nat s2, Add s1 s2 s3) => 
        FSVec s1 a -> FSVec s2 a -> FSVec s3 a
(++) = liftV2 (P.++)

infixl 5 <+
infixr 5 ++

-- | Apply a function on all elements of a vector
map :: Nat s => (a -> b) -> FSVec s a -> FSVec s b
map f = liftV (P.map f)

-- | Applies function pairwise on two vectors
zipWith :: Nat s => (a -> b -> c) -> FSVec s a -> FSVec s b -> FSVec s c
zipWith f = liftV2 (P.zipWith f)
 
-- | Folds a function from the right to the left  over a vector using an
--   initial value.
foldl :: Nat s => (a -> b -> a) -> a -> FSVec s b -> a 
foldl f e = (P.foldl f e) . unFSVec

-- | Folds a function from the left to the right over a vector using an 
--   initial value.
foldr :: Nat s => (b -> a -> a) -> a -> FSVec s b -> a
foldr f e = (P.foldr f e) . unFSVec

-- 'filter' takes a predicate function and a vector and creates a new vector 
--   with the elements for which the predicate is true. 
-- filterV :: (a -> Bool) -> Vector a -> Vector a
-- FIXME: 
--  Imposible to define, the result does not have a predictable static size


-- | zip two vectors into a vector of tuples.
zip :: Nat s => FSVec s a -> FSVec s b -> FSVec s (a, b)
zip = liftV2 P.zip
 
-- | unzip a vector of tuples into two vectors.
unzip :: Nat s => FSVec s (a, b) -> (FSVec s a, FSVec s b)
unzip (FSVec xs) = let (a,b) = P.unzip xs in (FSVec a, FSVec b)

-- | shift a value from the left into a vector. 
shiftl :: Pos s => FSVec s a -> a -> FSVec s a 
-- This doesn't work
-- shiftl xs x = x +> init xs
shiftl xs x = liftV ((x:) . P.init) xs

-- | shift a value from the left into a vector. 
shiftr :: Pos s => FSVec s a -> a -> FSVec s a 
-- This doesn't work
-- shiftr xs x = tail xs <+ x
shiftr xs x = liftV (P.tail . (P.++[x])) xs

-- | Rotate a vector to the left. Note that this fuctions does not change the 
--   size of a vector.
rotl :: forall s a . Nat s => FSVec s a -> FSVec s a
-- This doesn't work (it's highly inneficient anyway)
-- rotl [] = []
-- rotl vs    = lastV vs +> initV vs 
rotl = liftV rotl'
  where vl = toInt (undefined :: s)
        rotl' [] = []
        rotl' xs = let (i,[l]) = splitAt (vl - 1) xs   
                   in l : i
  
-- | Rotate a vector to the left. Note that this fuctions does not change the 
--   size of a vector.
rotr :: Nat s => FSVec s a -> FSVec s a
-- This doesn't work 
-- rotr [] = []
-- rotr vs    = tailV vs <: headV vs
rotr = liftV rotr'
  where rotr' [] = []
        rotr' l@(x:_) = P.tail l P.++ [x] 
        

-- |  flatten a vector of vectors to a single vector
concat :: (Nat s1, Nat s2, Nat s3, Mul s1 s2 s3) =>
          FSVec s1 (FSVec s2 a) -> FSVec s3 a
-- this won't work: 
-- concat = foldr (++) empty
concat = liftV (P.foldr ((P.++).unFSVec) [])
 
-- | reverse a vector
reverse :: Nat s => FSVec s a -> FSVec s a
reverse = liftV P.reverse


-- | generate a vector with a given number of elements starting from an 
-- initial element using a supplied function for the generation of elements. 
--
-- > FSVec> iterate d5 (+1) 1
--
-- > <1,2,3,4,5> :: Num a => FSVec D5 a
iterate :: Nat s => s -> (a -> a) -> a -> FSVec s a
iterate s f x = let s' = toInt s in FSVec (P.take s' $ P.iterate f x)


-- | 'generate' behaves in the same way as 'iterate', but starts with the 
-- application of the supplied function to the supplied value. 
--
-- > Vector> generate d5 (+1) 1
-- 
-- > <2,3,4,5,6> :: Num a => FSVec  D5 a
generate :: Nat s => s -> (a -> a) -> a -> FSVec s a
generate s f x = let s' = toInt s in FSVec (P.take s' $ P.tail $ P.iterate f x)


-- | generates a vector with a given number of copies of the same element. 
--
-- > Vector> copy d7 5 
-- 
-- > <5,5,5,5,5,5,5> :: Vector Integer
copy :: Nat s => s -> a -> FSVec s a
copy s x = iterate s id x

------------
-- Instances
------------

instance Nat s => DF.Foldable (FSVec s) where
 foldr = foldr
 
instance Nat s => Functor (FSVec s) where
 fmap = map

instance Nat s => DT.Traversable (FSVec s) where 
  traverse f = (fmap FSVec).(DT.traverse f).unFSVec

instance (Lift a, Nat s) => Lift (FSVec s a) where
 lift (FSVec xs) = [| unsafeFSVecCoerce $(undefSigE lengthType) (FSVec xs) |]
    where -- Get the vector length in a type-level decimal
         lengthType :: TypeQ
         lengthType = dec2TypeLevel $ toInt (undefined :: s)

---------------------
-- Internal functions
---------------------

-- the FSVec equivalent of liftM
-- note it is unsafe and shouldn't be exported
liftV :: ([a] -> [b]) -> FSVec s a -> FSVec s' b
liftV f  =  FSVec . f . unFSVec

-- the FSVec equivalent of liftM
-- note it is unsafe and shouldn't be exported
liftV2 :: ([a] -> [b] -> [c]) -> FSVec s1 a -> FSVec s2 b -> FSVec s3 c
liftV2 f a b = FSVec (f (unFSVec a) (unFSVec b))
  
-- version of splitAt which checks if the list contains enough elements
splitAtM :: Int -> [a] -> Maybe ([a],[a])
splitAtM n xs = splitAtM' n [] xs 
    where splitAtM' 0 xs ys = Just (xs,ys)
          splitAtM' n xs (y:ys) | n > 0 = do
            (ls,rs) <- splitAtM' (n - 1) xs ys
            return (y:ls,rs)
          splitAtM' _ _ _ = Nothing

-- Arbitraly coerce the length parameter of a vector
unsafeFSVecCoerce :: s' -> FSVec s a -> FSVec s' a
unsafeFSVecCoerce _ (FSVec v) = (FSVec v)

-- Obtain a TH expression of undefined coerced to certain type
undefSigE :: TypeQ -> ExpQ
undefSigE t = sigE [| undefined |] t

-- unsafely (a trusted length is provided) create a vector using CPS style
unsafeVectorCPS :: forall a w . Int -> [a] -> 
                                (forall s . Nat s => FSVec s a -> w) -> w
unsafeVectorCPS l xs f = reifyIntegral l 
                      (\(_ :: lt) -> f ((FSVec xs) :: (FSVec lt a)))
 
