module Main
  ( main
  )
where

import           Dhall
import           Salvador

import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain integrationTests

integrationTests :: TestTree
integrationTests = testGroup
  "Integration tests"
  [ testCase "Load file from Dhall" $ do
      spec <- input specType "./examples/petstore/salvador.dhall"
      specTitle spec @?= "Petstore"
  ]
