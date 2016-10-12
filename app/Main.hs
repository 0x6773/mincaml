module Main where

import           Options.Applicative

data MainOpts
  = MainOpts
  { inline :: Maybe Int
  , iter   :: Maybe Int
  , files  :: [FilePath]
  } deriving (Show)

optParser :: Parser MainOpts
optParser
  = MainOpts
  <$> optional (read <$> strOption optInline)
  <*> optional (read <$> strOption optIter)
  <*> optFiles
  where optInline
          = long "inline"
          <> metavar "INLINE"
          <> help "max. size of functions inlined"
        optIter
          = long "iter"
          <> metavar "ITER"
          <> help "max. no. of optimizations iterated"
        optFiles
          = some (argument str (metavar "INPUT-FILES"))

main :: IO ()
main = execParser opts >>= mainDispatcher
  where opts = info (helper <*> optParser) desc
        desc
          = fullDesc
          <> progDesc "Compile INPUT-FILES... (all .ml) to SPARC assembly (.s)"
          <> header "mincaml - A compiler for a subset of ML"

mainDispatcher :: MainOpts -> IO ()
mainDispatcher = undefined
