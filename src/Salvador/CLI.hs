module Salvador.CLI
  ( cliMain
  )
where

import qualified Dhall
import qualified Data.Text.IO                  as Text
import           Salvador.Spec
import           Salvador.Markdown

generateDocs :: IO ()
generateDocs = do
  contents <- Text.readFile "./salvador.dhall"
  spec     <- Dhall.input specType contents
  Text.writeFile "./api/README.md" (renderDocumentationGFM spec)

cliMain :: IO ()
cliMain = generateDocs
