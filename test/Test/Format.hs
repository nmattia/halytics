module Main (main) where

import Control.Monad        (forM_, unless)
import System.FilePath.Glob (glob)

main :: IO ()
main = glob "src/**/*.hs" >>= lineCheck

lineCheck :: [FilePath] -> IO ()
lineCheck fps = do
  sss <- mapM (\fp -> longLines fp >>= \ls -> return (fp,ls)) fps
  let ss = filter (\(fp, ls) -> not $ null ls) sss
  unless (null ss) $ do
      putStrLn "Some lines exceed 80 chars"
      forM_ ss $ \(fp, ls) -> do
        putStrLn $ "    (" ++ fp ++ ")"
        forM_ ls $ \l -> putStrLn $ "        -> " ++ l
      error "Bad formatting"

longLines :: FilePath -> IO [String]
longLines fp = filter (\l -> length l > 80) . lines <$> readFile fp
