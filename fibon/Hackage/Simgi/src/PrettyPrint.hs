{-----------------------------------------------------------------
 
  (c) 2008-2009 Markus Dittrich,
      National Resource for Biomedical Supercomputing &
      Carnegie Mellon University
 
 
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

-- | PrettyPrint provides tools for colored output to the terminal
module PrettyPrint ( Color(..)
                   , Intensity(..)
                   , color_string
                   , color_string_int
                   ) where


-- imports
import Prelude


-- define colors
data Color = Black | Red | Green | Yellow | Blue | Magenta 
           | Cyan | White | Reset 
           deriving(Enum)


-- define intensity 
data Intensity = Normal | Bold
               deriving(Eq)


-- convert a color into the corresponding color code string
get_color_code :: Color -> String
get_color_code = show . fromEnum 


-- convert an intensity to the corresponding color code string
get_intensity_code :: Intensity -> String
get_intensity_code x
  | x == Normal  = "22"
  | x == Bold    = "1"
  | otherwise    = "1"


-- convenience wrapper around color string for bold colors
color_string :: Color -> String -> String
color_string = color_string_int Bold
                

-- convert a standard string into one graphically rendered
-- allows customization of color and intensity
color_string_int :: Intensity -> Color -> String -> String
color_string_int intensity col str = 
    "\ESC[" 
    ++ (get_intensity_code intensity) 
    ++ ";3" 
    ++ (get_color_code col) 
    ++ "m"
    ++ str 
    ++ "\ESC[0;m"
