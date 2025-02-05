{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (forM_)
import System.FilePath.Glob (glob)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath (replaceExtension)
import Development.Shake.Command

unLiterateHaskell :: T.Text -> T.Text
unLiterateHaskell = T.unlines . map processLine . T.lines

processLine :: T.Text -> T.Text
processLine (T.stripEnd -> txt) = case T.uncons txt of
  Nothing -> txt
  Just ('>', txt) -> T.drop 1 txt -- may need to adjust here depending on style
  Just{} -> "-- " <> txt

main :: IO ()
main = do
  files <- glob "**/*.lhs"
  forM_ files $ \f -> do
    src <- T.readFile f
    T.writeFile (replaceExtension f "hs") (unLiterateHaskell src)
    Exit _ <- cmd ("mv" :: String) f (f <> ".bak")
    pure ()
