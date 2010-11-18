{-
   **************************************************************
   * Filename      : FileImport.hs                              *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 6 July, 2001                               *
   * Lines         : 25                                         *
   **************************************************************
-}

module FST.FileImport (open,saveToFile) where
import System.IO.Error (try)

open :: FilePath -> IO (Either String String)
open file = do res <- try (readFile file)
	       case res of
	        Right res -> return $ Right res
	        Left res  -> return $ Left $ "\nError:\tUnable to open \"" ++ file ++"\".\n"

saveToFile :: FilePath -> String -> IO (Either String ())
saveToFile file str = do res <- try (writeFile file str)
	                 case res of
	                  Right res -> return $ Right ()
	                  Left  res -> return $ Left $ "\nError:\tUnable to save to \"" ++ file ++"\".\n"
