module RealNum where

import Data.Hashable
import Data.Bits


data RealNum = RealNum Integer Int

instance Show RealNum where
  show (RealNum m e) = sign ++ int ++ '.' : frac where
    sign = if m < 0 then "-" else ""
    absm = if m < 0 then (-m) else m
    int  = show $ shift absm e
    frac = if e < 0 then show $ shift (absm * 10^fracLen) e else "0"
    fracLen = head $ filter fracLenCriteria [1..]
    fracLenCriteria n = (absm * 10^n) .&. (shift 1 (-e) - 1) == 0

instance Eq RealNum where
  a@(RealNum am ae) == b@(RealNum bm be)
    =  ae == be && am == bm
    || show a == show b

instance Hashable RealNum where
  hashWithSalt salt = hashWithSalt salt . show
  hash = hash . show

-- TODO instances
