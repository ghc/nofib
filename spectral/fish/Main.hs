module Main
where
import Vector	
import Fun_geom
--import AttrModule
--import Xview_Interface


run :: [(Int,Int,Int,Int)]
run = squarelimit (0, 0) (640, 0) (0,640)

-- partain: avoid X11
main = putStr (shows run "\n")

{-
roundrun = [Draw_line a b c d | (a, b, c, d) <- run]

main i
  = [Echo False,
     ReadChan stdin, 
     AppendChan stderr "Fish in Haskell\n",
     AppendChan stdout (prg i)]

prg i = unlines (map shw (prog i))

prog ~(_ : ~(~(Str x) : (Success : rest)))
  = [Xv_create 0 FRAME [],
     Xv_create base_win PANEL [],
     Xv_create panel_win PANEL_BUTTON [PANEL_LABEL_STRING "Start Fish"],
     Window_fit_height panel_win,
     Xv_create base_win CANVAS [],
     Window_fit canvas_win,
     Xv_main_loop base_win
     ] ++ others (drop 3 rep) 1
    where
    rep = (map read (lines x))::[Xview_reply]
    WinRef base_win = rep!!0
    WinRef panel_win = rep!!1
    WinRef canvas_win = rep!!2

others ~(x:xs) i 
  = if (x == PanelEvent 3) then 
      roundrun ++ Return:others xs (i+1)
    else 
      [Draw_line 0 i 500 i,Return] ++ others xs (i+2)
-}
