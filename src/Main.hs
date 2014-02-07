
module Main where

import Data.List (transpose)

type Binary = Bool
type Wire = [Binary] -- A list of discrete states
type ByteWire = [[Binary]] -- A list of simultaneous discrete states


oneToOne :: (Binary -> Binary) -> Wire -> Wire
oneToOne op = map op

wire = oneToOne id
notGate = oneToOne not

manyToOne :: ([Binary] -> Binary) -> [Wire] -> Wire
manyToOne op = map op . transpose

norGate = notGate . orGate
orGate = manyToOne or

nandGate = notGate . andGate
andGate = manyToOne and

xnorGate = notGate . xorGate
xorGate = manyToOne $ xor
  where
    xor = foldr1 (/=)

nequalsGate = notGate . equalsGate
equalsGate = manyToOne $ allSame
  where
    allSame [] = True
    allSame (x:xs) = all (== x) xs

testOneToOne g = g [True, False]
testManyToOne g =
  let w1 = [True, True, True, True, False, False, False, False]
      w2 = [True, True, False, False, True, True, False, False]
      w3 = [True, False, True, False, True, False, True, False]
  in  g [w1, w2, w3]

