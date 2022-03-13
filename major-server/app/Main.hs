module Main where

import Options.Applicative

import Domain (AccessToken(AccessToken))
import Server (launch)
import MemoryDatabase (memoryDatabaseFromFile)

data ServerConfig = ServerConfig {
  database :: String,
  adminToken :: String,
  port :: Int
}

opts :: ParserInfo ServerConfig
opts = info (serverConfig <**> helper)
  ( fullDesc <>
    progDesc
      ( "Launch the server on port PORT using the database in FILE and TOKEN as the" ++
        " administrator token" ) <>
    header "major – a backend for the Major mobile application" )
    where
      serverConfig = ServerConfig
        <$> strOption
          ( long "database" <>
            short 'd' <>
            metavar "FILE" <>
            help "File containing the initial database" )
        <*> strOption
          ( long "adminToken" <>
            short 'a' <>
            metavar "TOKEN" <>
            help "Access token for the administrator" )
        <*> option auto
          ( long "port" <>
            short 'p' <>
            metavar "PORT" <>
            help "Port on which the server must listen" )

main :: IO ()
main = do
  ServerConfig file token port <- execParser opts
  db <- memoryDatabaseFromFile file $ AccessToken token
  launch db port