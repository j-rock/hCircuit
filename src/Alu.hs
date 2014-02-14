module Alu where

import Main
import Data.List (transpose)

mux4test = do
    c0 <- allValues
    c1 <- allValues
    a  <- allValues
    b  <- allValues
    c  <- allValues
    d  <- allValues
    return (a, b, c, d, c0, c1, mux4 c0 c1 a b c d)
  where
    allValues = [[True], [False]]


mux2 control a b =
  let nc  = notGate control
      a'  = andGate [nc, a]
      b'  = andGate [control, b]
      out = orGate [a', b']
  in out

mux4 c0 c1 a b c d =
  let ab  = mux2 c0 a b
      cd  = mux2 c0 c d
      out = mux2 c1 ab cd
  in out

logicUnit c0 c1 a b =
  let and' = andGate [a, b]
      or'  = orGate [a, b]
      nor' = norGate [a, b]
      xor' = xorGate [a, b]
      out  = mux4 c0 c1 and' or' nor' xor'
  in out

fullAdder a b cin =
  let ps   = xorGate [a, b]
      sum  = xorGate [ps, cin]
      pc1  = andGate [a, b]
      pc2  = andGate [ps, cin]
      cout = orGate [pc1, pc2]
  in (sum, cout)
  
alu1 c0 c1 c2 a b cin =
  let b'         = xorGate [b, c0]
      (ar, cout) = fullAdder a b' cin
      logic      = logicUnit c0 c1 a b
      out        = mux2 c2 ar logic
  in (out, cout)

alu :: Wire -- Control Bit 0
    -> Wire -- Control Bit 1
    -> Wire -- Control Bit 2
    -> Bytewire -- N-Bit Wire
    -> Bytewire -- N-Bit Wire
    -> (Bytewire, Wire, Wire, Wire)
alu c0 c1 c2 a b =
  let (out, cout) = unzip (zipWith3 (alu1 c0 c1 c2) a b cin)
      cin         = c0 : cout
      overflow = xorGate . take 2 . reverse $ cout
    --TODO: implement int2Wire
    --zero     = int2Wire "32b0"
    --isZero   = equalsGate [out, zero]
      isZero   = map (all not) out
      negative = last out
  in (out, overflow, isZero, negative)

