{-# LANGUAGE BangPatterns, LambdaCase #-}
import Data.Bits
import Data.Word (Word32)
import Debug.Trace (traceShow)
-- et tu brute
nums n = length [(a,b,c)|a<-[1..n],b<-[a..n],c<-[b..n],a+b+c<=n,(gcd a b `gcd` c) == 1]
nums' :: Int -> (Int,Int,Int)
nums' n = last [(a,b,c)|a<-[1..n],b<-[a..n],c<-[b..n],a+b+c<=n]


-- here hold this
--binaryGCDImpl a b =
--    case shiftToOddCount a of
--      (!za, !oa) ->
--        case shiftToOddCount b of
--          (!zb, !ob) -> gcdOdd (abs oa) (abs ob) `shiftL` min za zb

--{-# INLINE gcdOdd #-}
--gcdOdd :: Word32 -> Word32 -> Word32
--gcdOdd a b
--  | a == 1 || b == 1    = 1
--  | a < b               = oddGCD b a
--  | a > b               = oddGCD a b
--  | otherwise           = a

--oddGCD :: Word32 -> Word32 -> Word32
--oddGCD a b =
--    case shiftToOdd (a-b) of
--      1 -> 1
--      c | c < b     -> oddGCD b c
--        | c > b     -> oddGCD c b
--        | otherwise -> c

-- returns 1 if even
{-#INLINE beven #-}
beven :: Word32 -> Bool
beven n = case complement n .&. 1 of
  1 -> True
  0 -> False

-- returns 1 if odd
{-#INLINE bodd #-}
bodd :: Word32 -> Bool
bodd n = case n .&. 1 of 
  1 -> True
  0 -> False

-- right shift 1
{-#INLINE rs1 #-}
rs1 :: Word32 -> Word32
rs1 n = shiftR n 1


-- left shift 1
{-#INLINE ls1 #-}
ls1 :: Word32 -> Word32
ls1 n = shiftL n 1

bics :: Word32 -> String
bics n = reverse $ concatMap (show . toB . testBit n) [0..31]
  where 
    toB = \case
      True  -> 1
      False -> 0


bgcd u v | u == v  = u
         | u == 0  = v
         | v == 0  = u
         | beven u = case bodd v of
            True  -> bgcd (rs1 u) v
            False -> ls1 $ bgcd (rs1 u) (rs1 v) 
         | bodd u  = bgcd u (rs1 v)
         | u > v   = bgcd (rs1 (u-v)) v
         | True    = bgcd (rs1 (v-u)) u
{-
unsigned int gcd(unsigned int u, unsigned int v)
{
    // simple cases (termination)
    if (u == v)
        return u;

    if (u == 0)
        return v;

    if (v == 0)
        return u;

    // look for factors of 2
    if (~u & 1) // u is even
    {
        if (v & 1) // v is odd
            return gcd(u >> 1, v);
        else // both u and v are even
            return gcd(u >> 1, v >> 1) << 1;
    }

    if (~v & 1) // u is odd, v is even
        return gcd(u, v >> 1);

    // reduce larger argument
    if (u > v)
        return gcd((u - v) >> 1, v);

    return gcd((v - u) >> 1, u);
}

-}