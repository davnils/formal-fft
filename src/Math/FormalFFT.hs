{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}

module Math.FormalFFT where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Monad (join)
import Data.Monoid ((<>))
import Data.SBV
import Data.Tuple (swap)
import Prelude hiding (length)
import Test.QuickCheck

type Complex = (SReal, SReal)

cadd,cmul,csub :: Complex -> Complex -> Complex
(a,b) `cadd` (a', b') = (a + a', b + b')
(a,b) `csub` (a', b') = (a - a', b - b')
(a,b) `cmul` (a', b') = (a*a' - b*b', b*a' + a*b')

invmul :: SInteger -> Complex -> Complex
invmul k (a,b) = let k' = 1/toSReal k in (k' * a,  -k' * b)

invmulk :: SInteger -> Complex -> Complex
invmulk k (a,b) = let k' = 1/toSReal k in (k' * a, k' * b)

pow :: Complex -> SInteger -> Complex
pow c n = ite (n.== 0) (1, 0) $ ite (n .== 1) c $ c `cmul` pow c (n - 1)

constrainPrimitiveUnitRoot :: Complex -> SInteger -> Symbolic ()
constrainPrimitiveUnitRoot c n = do
  constrain $ pow c n .== (1, 0)
  go c (n - 1)

  where
  go _ 0 = return ()
  go c n' = constrain (pow c n' ./= (1, 0)) >> go c (n' - 1)

constrainTransform :: Complex -> Int -> Symbolic SBool
constrainTransform c np = do
  work1 <- mkFreeVars np
  work2 <- mkFreeVars np

  let work  = zip work1 work2
      shared = dft false c work
      dft'  = dft true c shared
      fft'  = fft true c (fft false c work)
      mixed = fft true c shared

  return $ dft' .== work &&& mixed .== work &&& fft' .== work

proveTransform :: Int -> IO ThmResult
proveTransform np = do
  let n = literal $ fromIntegral np

  prove $ do
    [real, imag] <- {- mkExistVars 2 -} symbolics ["c_re", "c_im"]
    let c = (real, imag)
    constrainPrimitiveUnitRoot c n >> constrainTransform c np

testTransform :: Int -> IO ()
testTransform np = do
  let n = literal $ fromIntegral np

  res <- sat $ do
    [real, imag] <- mkExistVars 2
    constrainPrimitiveUnitRoot (real,imag) n
    return $ (literal true :: SBool)

  let (Just (r1, r2)) = extractModel res
      r1' = literal r1
      r2' = literal r2
  putStrLn $  "* " <> show np <> ", running with root: " <> show (r1', r2')

  print $ isConcrete r1'
  print $ isConcrete r2'

  quickCheck $ constrainTransform (r1', r2') np

dft :: SBool -> Complex -> [Complex] -> [Complex]
dft invert root x = go x 0
  where
  powers c n = ite (n .== inputLength) [] (pow c n : powers c (n + 1))
  powers' = powers root 0
  inputLength = length x

  go [] _ = []
  go (_:rest) row = y_i : go rest (row + 1)
    where
    rowPowers = map (\c -> ite invert (inputLength `invmul` pow c row) (pow c row)) powers'
    y_i = x `dot` rowPowers

length :: [a] -> SInteger
length = foldr (const (+1)) 0

dot :: [Complex] -> [Complex] -> Complex
dot [] [] = (0, 0)
dot (a:at) (b:bt) = (a `cmul` b) `cadd` dot at bt

fft :: SBool -> Complex -> [Complex] -> [Complex]
fft inverse root x = map (invmulk scaling) $ go root x
  where
  scaling = ite inverse (length x) 1
  exponent = ite inverse (-2) 2
  go :: Complex -> [Complex] -> [Complex]
  go _ (a:[]) = [a]
  go c a = uncurry (<>) $ unzip combined

    where
    (subresult_1, subresult_2) = join (***) (go $ pow c exponent) $ divideEvenOdd a
    rootPowers = iterate (`cmul` c) (1, 0)
    combined = zipWith3 (\y_i y_i' z -> (y_i `cadd` (z `cmul` y_i'), y_i `csub` (z `cmul` y_i')))
               subresult_1 subresult_2 (ite inverse (map (invmul 1) rootPowers) rootPowers)

divideEvenOdd :: [a] -> ([a], [a])
divideEvenOdd = swap . foldr (\e (l1, l2) -> (l2, e:l1)) ([], [])
