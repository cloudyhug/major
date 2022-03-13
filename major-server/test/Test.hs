module Test where

import Domain (AccessToken(AccessToken))
import Server (launch)
import MemoryDatabase (memoryDatabaseFromFile)

main :: IO ()
main = do
  db <- memoryDatabaseFromFile "example_config.json" $ AccessToken "42"
  launch db 8000