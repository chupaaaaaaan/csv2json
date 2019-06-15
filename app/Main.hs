module Main where

import qualified Data.Text as T
import My.Data.Parser.Csv

main :: IO ()
main = do
  cs <- T.pack <$> readFile "test.csv"
  case parseCsv cs of
    Left l -> putStrLn l
    Right r -> print r
