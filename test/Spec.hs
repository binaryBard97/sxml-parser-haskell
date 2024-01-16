module Main (main) where

import Parser
import PrettyPrinter
import SXML hiding (main)
import Spec.Util
import Test.Tasty

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests =
  testGroup
    "Prog08"
    <$> sequence
      [ sxmlParseTests,
        sxmlPrettyPrintTests
      ]

-- ###################################################################
-- ###################################################################

sxmlParseTests :: IO TestTree
sxmlParseTests = do
  testData :: [(String, String, Maybe SXML)] <-
    do
      testData <- read <$> readFile "test/TestData/SXMLParseTests.dat"
      mapM (\(f, res) -> (f,,res) <$> readFile f) testData
  return $
    testGroup "sxmlP" $
      genTestCases (map mkSXMLParseTest testData)
  where
    mkSXMLParseTest (f, s, res) =
      mkAssertEqualExpr mkExpr (parseMaybe sxmlP . snd) (f, s) res
      where
        mkExpr (f, _) =
          "parseMaybe sxmlP <$> readFile " ++ show f

sxmlPrettyPrintTests :: IO TestTree
sxmlPrettyPrintTests = do
  sxmls :: [SXML] <-
    read <$> readFile "test/TestData/SXMLs.dat"
  return $
    testGroup "sxmlD" $
      genTestCases (concatMap mkSXMLPrettyPrintTests sxmls)
  where
    mkSXMLPrettyPrintTests sxml =
      [ mkSXMLPrettyPrintTest3 sxml,
        mkSXMLPrettyPrintTest1 sxml,
        mkSXMLPrettyPrintTest2 80 sxml,
        mkSXMLPrettyPrintTest2 40 sxml,
        mkSXMLPrettyPrintTest2 0 sxml
      ]
    mkSXMLPrettyPrintTest1 sxml =
      mkAssertEqualExpr mkExpr (parseMaybe sxmlP . render . sxmlD) sxml (Just sxml)
      where
        mkExpr sxml =
          "parseMaybe sxmlP (render (sxmlD " ++ showsPrec 11 sxml "" ++ "))"
    mkSXMLPrettyPrintTest2 w sxml =
      mkAssertEqualExpr mkExpr (\(w, sxml) -> parseMaybe sxmlP (pretty w (sxmlD sxml))) (w, sxml) (Just sxml)
      where
        mkExpr (w, sxml) =
          "parseMaybe sxmlP (pretty " ++ showsPrec 11 w "" ++ " (sxmlD " ++ showsPrec 11 sxml "" ++ "))"
    mkSXMLPrettyPrintTest3 sxml =
      mkAssertEqualExpr mkExpr (\sxml -> let doc = sxmlD sxml in length (render doc) <= 25 || length (filter (== '\n') (pretty 40 doc)) > 1) sxml True
      where
        mkExpr sxml =
          "let doc = sxmlD " ++ showsPrec 11 sxml "" ++ " in length (render doc) <= 25 || length (filter (== '\n') (pretty 40 doc)) > 1"
