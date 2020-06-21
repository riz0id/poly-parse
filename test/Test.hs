{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |

import           Control.Algebra
import           Control.Carrier.Parser
import Control.Effect.Parser
import           Control.Effect.Lift
import           Control.Monad.IO.Class
import           Data.Parser.Err
import           Data.Parser.Input
import           Data.Source
import           Data.Text
import           Parsers
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = do
  runParserTest "a" testParseError
  defaultMain $ testGroup "unit tests"
    [ parserTests
    ]

testParseError :: Has Parser sig m => m Parens
testParseError = lparen <!> "lala"

parserTests :: TestTree
parserTests = testGroup "ParserC (Church)"
  [ parensTest
  , symbolTest
  , digitTest
  ]

sexpTest :: TestTree
sexpTest = testGroup "ParserC SExps"
  [ testCase "SExp" $ do
      result <- runParserTest "(a (b 12))" sexp
      result @?=
        [ TParens LParens
        , TSym (pack "a")
        , TParens LParens
        , TSym (pack "b")
        , TNum (WholeNum (pack "12"))
        , TParens RParens
        , TParens RParens
        ]
  ]

digitTest :: TestTree
digitTest = testGroup "ParserC digits"
  [ testCase "Digit - Decimal" $ do
      result <- runParserTest "12.2" digit
      result @?= Decimal (pack "12") (pack "2")

  , testCase "Digit - Whole numbers" $ do
      result <- runParserTest "23" digit
      result @?= WholeNum (pack "23")
  ]

symbolTest :: TestTree
symbolTest = testGroup "ParserC symbols"
  [ testCase "Symbols - a-z" $ do
      result <- runParserTest "a" symbol
      result @?= TSym (pack "a")

  , testCase "Symbols - Z-2a-3" $ do
      result <- runParserTest "Z-2a-3" symbol
      result @?= TSym (pack "Z-2a-3")
  ]

parensTest :: TestTree
parensTest = testGroup "ParserC Parenthesis"
  [ testCase "Left Parenthesis"  $ do
      result <- liftIO $ runParserTest "(" lparen
      result @?= LParens
  , testCase "Right Parenthesis" $ do
      result <- runParserTest ")" rparen
      result @?= RParens
  ]

runParserTest :: Has (Lift IO) sig m => String -> ParserC String m a -> m a
runParserTest input =
  runParser (const pure) (error . show . errExpected) (Input emptyPos mempty input)
