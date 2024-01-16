module Spec.Util
  ( genTestCasesAtWidth,
    genXtraCasesAtWidth,
    genTestCases,
    genXtraCases,
    assertEqualExpr,
    mkAssertEqualExprIO,
    mkAssertEqualExpr,
    genAssertEqualExpr,
    assertEqualBy,
    assertEqualByExpr,
    mkAssertEqualByExprIO,
    mkAssertEqualByExpr,
    genAssertEqualByExpr,
  )
where

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

labelCasesAtWidth :: Int -> String -> [Assertion] -> [TestTree]
labelCasesAtWidth width prefix = zipWith (testCase . printf (prefix ++ "%0" ++ show width ++ "d")) [1 :: Integer ..]

labelCases :: String -> [Assertion] -> [TestTree]
labelCases prefix assertions = labelCasesAtWidth (log10 (length assertions)) prefix assertions
  where
    log10 n = if n < 10 then 1 else 1 + log10 (n `div` 10)

genTestCasesAtWidth :: Int -> [Assertion] -> [TestTree]
genTestCasesAtWidth width = labelCasesAtWidth width "test"

genTestCases :: [Assertion] -> [TestTree]
genTestCases = labelCases "test"

genXtraCasesAtWidth :: Int -> [Assertion] -> [TestTree]
genXtraCasesAtWidth width = labelCasesAtWidth width "xtra"

genXtraCases :: [Assertion] -> [TestTree]
genXtraCases = labelCases "xtra"

assertEqualExpr :: (Eq a, Show a, HasCallStack) => String -> a -> a -> Assertion
assertEqualExpr expr = assertEqual ("    expr: " ++ expr)

mkAssertEqualExprIO :: (Eq b, Show b, HasCallStack) => (a -> String) -> (a -> IO b) -> a -> b -> Assertion
mkAssertEqualExprIO mkExpr mkActual arg expected = do
  actual <- mkActual arg
  assertEqualExpr (mkExpr arg) expected actual

mkAssertEqualExpr :: (Eq b, Show b, HasCallStack) => (a -> String) -> (a -> b) -> a -> b -> Assertion
mkAssertEqualExpr mkExpr mkActual =
  mkAssertEqualExprIO mkExpr $ pure . mkActual

genAssertEqualExpr :: (Eq b, Show b, HasCallStack) => (a -> String) -> (a -> b) -> [(a, b)] -> [Assertion]
genAssertEqualExpr mkExpr mkActual = map (uncurry $ mkAssertEqualExpr mkExpr mkActual)

assertEqualBy :: (Show a, HasCallStack) => String -> (a -> a -> Bool) -> String -> a -> a -> Assertion
assertEqualBy note eq preface expected actual =
  unless (eq actual expected) (assertFailure msg)
  where
    msg =
      (if null preface then "" else preface ++ "\n")
        ++ "expected: "
        ++ show expected
        ++ "\n but got: "
        ++ show actual
        ++ (if null note then "" else "\n    note: " ++ note)

assertEqualByExpr :: (Show a, HasCallStack) => String -> (a -> a -> Bool) -> String -> a -> a -> Assertion
assertEqualByExpr note eq expr = assertEqualBy note eq ("    expr: " ++ expr)

mkAssertEqualByExprIO :: (Show b, HasCallStack) => String -> (b -> b -> Bool) -> (a -> String) -> (a -> IO b) -> a -> b -> Assertion
mkAssertEqualByExprIO note eq mkExpr mkActual arg expected = do
  actual <- mkActual arg
  assertEqualByExpr note eq (mkExpr arg) expected actual

mkAssertEqualByExpr :: (Show b, HasCallStack) => String -> (b -> b -> Bool) -> (a -> String) -> (a -> b) -> a -> b -> Assertion
mkAssertEqualByExpr note eq mkExpr mkActual =
  mkAssertEqualByExprIO note eq mkExpr (return . mkActual)

genAssertEqualByExpr :: (Show b, HasCallStack) => String -> (b -> b -> Bool) -> (a -> String) -> (a -> b) -> [(a, b)] -> [Assertion]
genAssertEqualByExpr note eq mkExpr mkActual = map (uncurry $ mkAssertEqualByExpr note eq mkExpr mkActual)
