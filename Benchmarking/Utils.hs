{-# LANGUAGE Safe #-}

module Benchmarking.Utils where

import Prelude (Float, fromInteger, (-), (/), (+))
import System.Time (ClockTime(TOD))

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)
