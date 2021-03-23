module Splint.Parser where

import qualified Control.Exception as Exception
import qualified Language.Haskell.HLint as HLint
import qualified Splint.GHC.Plugins as GHC

parse
  :: HLint.ParseFlags
  -> GHC.ModSummary
  -> GHC.HsParsedModule
  -> GHC.Hsc HLint.ModuleEx
parse parseFlags modSummary _ = GHC.liftIO $ do
  let filePath = GHC.msHsFilePath modSummary
  result <- HLint.parseModuleEx parseFlags filePath Nothing
  case result of
    Left parseError -> Exception.throwIO $ ParseError parseError
    Right moduleEx -> pure moduleEx

newtype ParseError = ParseError HLint.ParseError

instance Exception.Exception ParseError

instance Show ParseError where
  show (ParseError parseError) = mconcat
    [ "ParseError { parseErrorLocation = "
    , show $ HLint.parseErrorLocation parseError
    , ", parseErrorMessage = "
    , show $ HLint.parseErrorMessage parseError
    , ", parseErrorContents = "
    , show $ HLint.parseErrorContents parseError
    , " }"
    ]
