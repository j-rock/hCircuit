module Alu where

import Main
import Data.List (transpose)


mux2test = do
    c <- allValues
    a <- allValues
    b <- allValues
    return (a, b, c, mux2 c a b)
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

alu32 :: Wire -- Control Bit 0
      -> Wire -- Control Bit 1
      -> Wire -- Control Bit 2
      -> ByteWire -- 32 Bit Wire addend
      -> ByteWire -- 32 Bit Wire addend
      -> (ByteWire, Wire, Wire, Wire)
alu32 c0 c1 c2 a b =
  let aluc            = alu1 c0 c1 c2
      (out0, cout0)   = aluc (a !! 0) (b !! 0) c0
      (out1, cout1)   = aluc (a !! 1) (b !! 1) cout0
      (out2, cout2)   = aluc (a !! 2) (b !! 2) cout1
      (out3, cout3)   = aluc (a !! 3) (b !! 3) cout2
      (out4, cout4)   = aluc (a !! 4) (b !! 4) cout3
      (out5, cout5)   = aluc (a !! 5) (b !! 5) cout4
      (out6, cout6)   = aluc (a !! 6) (b !! 6) cout5
      (out7, cout7)   = aluc (a !! 7) (b !! 7) cout6
      (out8, cout8)   = aluc (a !! 8) (b !! 8) cout7
      (out9, cout9)   = aluc (a !! 9) (b !! 9) cout8
      (out10, cout10) = aluc (a !! 10) (b !! 10) cout9
      (out11, cout11) = aluc (a !! 11) (b !! 11) cout10
      (out12, cout12) = aluc (a !! 12) (b !! 12) cout11
      (out13, cout13) = aluc (a !! 13) (b !! 13) cout12
      (out14, cout14) = aluc (a !! 14) (b !! 14) cout13
      (out15, cout15) = aluc (a !! 15) (b !! 15) cout14
      (out16, cout16) = aluc (a !! 16) (b !! 16) cout15
      (out17, cout17) = aluc (a !! 17) (b !! 17) cout16
      (out18, cout18) = aluc (a !! 18) (b !! 18) cout17
      (out19, cout19) = aluc (a !! 19) (b !! 19) cout18
      (out20, cout20) = aluc (a !! 20) (b !! 20) cout19
      (out21, cout21) = aluc (a !! 21) (b !! 21) cout20
      (out22, cout22) = aluc (a !! 22) (b !! 22) cout21
      (out23, cout23) = aluc (a !! 23) (b !! 23) cout22
      (out24, cout24) = aluc (a !! 24) (b !! 24) cout23
      (out25, cout25) = aluc (a !! 25) (b !! 25) cout24
      (out26, cout26) = aluc (a !! 26) (b !! 26) cout25
      (out27, cout27) = aluc (a !! 27) (b !! 27) cout26
      (out28, cout28) = aluc (a !! 28) (b !! 28) cout27
      (out29, cout29) = aluc (a !! 29) (b !! 29) cout28
      (out30, cout30) = aluc (a !! 30) (b !! 30) cout29
      (out31, cout31) = aluc (a !! 31) (b !! 31) cout30

      out = transpose [out0, out1, out2, out3, out4, out5, out6, out7, out8
                      ,out9, out10, out11, out12, out13, out14, out15, out16
                      ,out17, out18, out19, out20, out21, out22, out23, out24
                      ,out25, out26, out27, out28, out29, out30, out31]
      overflow = xorGate [cout30, cout31]
    --TODO: implement uint2Wire
    --zero     = uint2Wire "32b0"
    --isZero   = equalsGate [out, zero]
      isZero   = map (all (==False)) out
      negative = out31
  in (out, overflow, isZero, negative)

