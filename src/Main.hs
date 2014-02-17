
module Main where

import Data.List (transpose)

type Binary = Bool
type Wire = [Binary] -- A list of discrete states
type Bytewire = [[Binary]] -- A list of simultaneous discrete states

wireValues = [[True], [False]]

wireToWire :: (Binary -> Binary) -> Wire -> Wire
wireToWire = map

testWireToWire g = do
  input <- wireValues
  return (input, g input)

wire = wireToWire id
notGate = wireToWire not

wiresToWire :: ([Binary] -> Binary) -> [Wire] -> Wire
wiresToWire op = map op . transpose

testWiresToWire g = do
  a <- wireValues
  b <- wireValues
  c <- wireValues
  return (a, b, c, g [a, b, c])

norGate = notGate . orGate
orGate = wiresToWire or

nandGate = notGate . andGate
andGate = wiresToWire and

xnorGate = notGate . xorGate
xorGate = wiresToWire xor
  where
    xor = foldr1 (/=)

nequalsGate = notGate . equalsGate
equalsGate = wiresToWire allSame
  where
    allSame [] = True
    allSame (x:xs) = all (== x) xs


