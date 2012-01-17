{-----------------------------------------------------------------
 
  (c) 2009-2010 Markus Dittrich 
 
  This program is free software; you can redistribute it 
  and/or modify it under the terms of the GNU General Public 
  License Version 3 as published by the Free Software Foundation. 
 
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License Version 3 for more details.
 
  You should have received a copy of the GNU General Public 
  License along with this program; if not, write to the Free 
  Software Foundation, Inc., 59 Temple Place - Suite 330, 
  Boston, MA 02111-1307, USA.

--------------------------------------------------------------------}

-- | RpnCalc defines the data structures and a calculator engine
-- for computing mathematical expressions that have been parsed
-- into reverse polish notations
module RpnData ( RpnItem(..)
               , RpnStack(..)
               ) where


-- imports 
import Prelude


-- | RpnItem describes all items that can be present in our
-- rpn stack
data RpnItem = Time
               | Number Double 
               | Variable String
               | UnaFunc (Double -> Double) 
               | BinFunc (Double -> Double -> Double)


-- | RpnStack describes a computation stored in a stack of
-- RpnItems
newtype RpnStack = RpnStack { toList :: [RpnItem] }
  deriving(Show) 

-- | make RpnStack an instance of Eq. 
-- We compare two RpnStacks by computing them and replacing
-- each occuring Variable by a default value. This should
-- be ok in all but some pathological cases.
instance Eq RpnStack where
  
  x == y  = (internal_rpn_compute x) == (internal_rpn_compute y)


-- | computes an expressions based on an rpn stack
-- NOTE: This function is intended to be used for
-- the Eq instance of RpnItem only!
-- names are replaced by defaultVar. 

-- | default variable used to replace all encountered
-- variable. Its value is arbitrary
defaultVar :: Double
defaultVar = 47.0

internal_rpn_compute :: RpnStack -> Double
internal_rpn_compute (RpnStack [(Number x)]) = x
internal_rpn_compute (RpnStack xs)           = num 

  where
    (Number num) = head . foldl evaluate [] $ xs

    -- evaluate unary function (sin, cos, ..)
    evaluate ((Number x):ys) (UnaFunc f) = 
      (Number $ f x):ys

    -- evaluate binary function (*,+,..)
    evaluate ((Number x):(Number y):ys) (BinFunc f) =
      (Number $ f y x):ys

    -- extrace current time
    evaluate ys (Time) = (Number defaultVar):ys

    -- extract variable name
    evaluate ys (Variable _) = (Number defaultVar):ys

    evaluate ys item = item:ys



-- | a pretty lame show instance
-- use only for debugging purposes
instance Show RpnItem where
  show (Time)       = "TIME"
  show (Number x)   = show x
  show (BinFunc x)  = show (x 1 2) 
  show (UnaFunc x)  = show (x 1)
  show (Variable x) = x



