{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Data.SBV
import Control.Monad
import Prelude hiding (length)

type Complex = (SReal, SReal)

cadd,cmul,csub :: Complex -> Complex -> Complex
invmul :: SInteger -> Complex -> Complex
(a,b) `cadd` (a', b') = (a + a', b + b')
(a,b) `csub` (a', b') = (a - a', b - b')
(a,b) `cmul` (a', b') = (a*a' - b*b', b*a' + a*b')
invmul k (a,b) = let k' = 1/toSReal k in (k' * a,  -k' * b)

pow :: Complex -> SInteger -> Complex
pow c n = ite (n.== 0) (1, 0) $ ite (n .== 1) c $ c `cmul` pow c (n - 1)

testTransform :: Int -> Symbolic SBool
testTransform np = do
  [real, imag] <- symbolics ["c_re", "c_im"]
  let c = (real, imag) :: Complex
      n = literal $ fromIntegral np
  constrain $ pow c n .== (1, 0)
  go c (n - 1)

  work1 <- mkFreeVars np
  work2 <- mkFreeVars np
  let work        = zip work1 work2

  return $ work .== dft true c (dft false c work)

  where
    go :: Complex -> SInteger -> Symbolic ()
    go _ 0 = return ()
    go c n' = constrain (pow c n' ./= (1, 0)) >> go c (n' - 1)

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

main :: IO ()
main = mapM_ (\n -> prove (testTransform n) >>= print) [1..4]
