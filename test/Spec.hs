import Test.HUnit
import System.Exit
import Lib
import Data.Char

-- item tests
testEmptyInput = TestCase (assertEqual "parse item \"\"" ([]) (parse item ""))
testItem = TestCase (assertEqual "parse item \"abc\"" ([('a', "bc")]) (parse item "abc"))

-- Functor (fmap) tests
testFunctorEmptyInput = TestCase (assertEqual "parse (fmap toLower item) \"\"" ([]) (parse (fmap toLower item) ""))
testFunctorLowerInput = TestCase (assertEqual "parse (fmap toLower item) \"ab\"" ([('a', "b")]) (parse (fmap toLower item) "ab"))
testFunctor = TestCase (assertEqual "parse (fmap toLower item) \"AB\"" ([('a', "B")]) (parse (fmap toLower item) "AB"))

-- Aplicative tests
testApplicativePure = TestCase (assertEqual "parse (pure 1) \"abc\"" ([(1,"abc")]) (parse (pure 1) "abc"))
testApplicative = TestCase (assertEqual "parse (pure toLower <*> item) \"ABC\"" ([('a',"BC")]) (parse (pure toLower <*> item) "ABC"))

-- Monad tests
testMonad = TestCase (assertEqual "parse (item >>= (\\s -> pure toLower s)) \"ABC\"" ([('a', "BC")]) (parse (item >>= (\s -> return (toLower s))) "ABC"))

-- do notation tests
testDoNotationEmptyInput = TestCase (assertEqual "parse p \"\"" ([]) (parse p ""))
testDoNotationSmallInput = TestCase (assertEqual "parse p \"ab\"" ([]) (parse p "ab"))
testDoNotation = TestCase (assertEqual "parse p \"abc\"" ([(('a','c'),"")]) (parse p "abc"))
testDoNotationLargeInput = TestCase (assertEqual "parse p \"abcd\"" ([(('a','c'),"d")]) (parse p "abcd"))

-- Parsers and Combinators tests
testSat = TestCase (assertEqual "parse (sat isLower) \"abc\"" ([('a', "bc")]) (parse (sat isLower) "abc"))
testSatFail = TestCase (assertEqual "parse (sat isLower) \"ABC\"" ([]) (parse (sat isLower) "ABC"))

testMany0EmptyInput = TestCase (assertEqual "parse (many0 (char 'a') \"\"" ([("","")]) (parse (many0 (char 'a')) ""))
testMany0Fail = TestCase (assertEqual "parse (many0 (char 'a') \"baa\"" ([("","baa")]) (parse (many0 (char 'a')) "baa"))
testMany0 = TestCase (assertEqual "parse (many0 (char 'a') \"aaa\"" ([("aaa","")]) (parse (many0 (char 'a')) "aaa"))
testMany0Extra = TestCase (assertEqual "parse (many0 (char 'a') \"aaabba\"" ([("aaa","bba")]) (parse (many0 (char 'a')) "aaabba"))

testMany1EmptyInput = TestCase (assertEqual "parse (many1 (char 'a') \"\"" ([]) (parse (many1 (char 'a')) ""))
testMany1Fail = TestCase (assertEqual "parse (many1 (char 'a') \"baa\"" ([]) (parse (many1 (char 'a')) "baa"))
testMany1 = TestCase (assertEqual "parse (many1 (char 'a') \"aaa\"" ([("aaa","")]) (parse (many1 (char 'a')) "aaa"))
testMany1Extra = TestCase (assertEqual "parse (many1 (char 'a') \"aaabba\"" ([("aaa","bba")]) (parse (many1 (char 'a')) "aaabba"))

testChar = TestCase (assertEqual "parse (char 'a') \"abc\"" ([('a', "bc")]) (parse (char 'a') "abc"))
testCharFail = TestCase (assertEqual "parse (char 'a') \"bc\"" ([]) (parse (char 'a') "bc"))

testString = TestCase (assertEqual "parse (string \"foo\") \"foobar\"" ([("foo", "bar")]) (parse (string "foo") "foobar"))
testStringEmptyInput = TestCase (assertEqual "parse (string \"foo\") \"\"" ([]) (parse (string "foo") ""))
testStringFail = TestCase (assertEqual "parse (string \"foo\") \"bar\"" ([]) (parse (string "foo") "bar"))

testSpace = TestCase (assertEqual "parse space \"   a\"" ([("   ", "a")]) (parse space "   a"))
testSymbPlus = TestCase (assertEqual "parse (symb \"+\") \" + \"" [("+","")] (parse (symb "+") " + "))

testDigit = TestCase (assertEqual "parse digit \"123\""([(1,"23")]) (parse digit "123"))
testDigitFail = TestCase (assertEqual "parse digit \"a123\""([]) (parse digit "a123"))

-- Arithmetic tests
testPlus = TestCase(assertEqual "1 + 2" ([(3, "")]) (apply expr " 1 + 2 "))
testMinus = TestCase(assertEqual "1 - 1" ([(0, "")]) (apply expr " 1 - 1 "))
testMultipy = TestCase(assertEqual "4 * 2" ([(8, "")]) (apply expr " 4 * 2 "))
testDivide = TestCase(assertEqual "4 / 2" ([(2, "")]) (apply expr " 4 / 2 "))
testAddMultipy = TestCase(assertEqual "1 + 4 * 2" ([(9, "")]) (apply expr " 1 + 4 * 2 "))
testAddDivide = TestCase(assertEqual "1 + 4 / 2" ([(3, "")]) (apply expr " 1 + 4 / 2 "))
testArithmeticLong = TestCase(assertEqual "1 + 2*3 - 4/5 - 6" ([(1, "")]) (apply expr " 1 + 2*3 - 4/5 - 6"))
testArithmeticParentheses = TestCase(assertEqual " (1 + 2) * 3 / (2 - 3)" ([(-9, "")]) (apply expr " (1 + 2) * 3 / (2 - 3) "))

-- CSV tests
testCsvFail = TestCase(assertEqual "ab,12,abc" ([]) (parse csv "ab,12,abc"))
testCsvString = TestCase(assertEqual "'ab'..." ([(Text "ab","...")]) (parse csv "'ab'..."))
testCsvNumber = TestCase(assertEqual "12..." ([(Number 12,"...")]) (parse csv "12..."))
testCsvNumAsString = TestCase(assertEqual "'12'..." ([(Text "12","...")]) (parse csv "'12'..."))
testRow = TestCase(assertEqual "'ab',12,'abc'\n" ([([Text "ab",Number 12,Text "abc"],"\n")]) (parse row "'ab',12,'abc'\n"))
testRowSpaces = TestCase(assertEqual " 'ab', 12, 'abc' \n" ([([Text "ab",Number 12,Text "abc"]," \n")]) (parse row " 'ab', 12, 'abc' \n"))
testRowFail = TestCase(assertEqual "ab, 12,'abc'\n" ([([],"ab, 12,'abc'\n")]) (parse row "ab, 12,'abc'\n"))
testRows = TestCase(assertEqual "1, 'ab', 11, 'ba'\n 2, 'cd', 22, 'dc'\n" ([([[Number 1,Text "ab",Number 11,Text "ba"],[Number 2,Text "cd",Number 22,Text "dc"]],"")]) (parse rows "1, 'ab', 11, 'ba'\n 2, 'cd', 22, 'dc'\n"))
testRowsFail = TestCase(assertEqual "1, 'ab', 11, 'ba' \n 2, 'cd', 22, 'dc' \n" ([]) (parse rows "1, 'ab', 11, 'ba' \n 2, 'cd', 22, 'dc' \n"))

main :: IO ()
main = do
  counts <- runTestTT (test
    [
      -- Test item
      testEmptyInput,
      testItem,

      -- Test the functor implementation
      testFunctorEmptyInput,
      testFunctorLowerInput,
      testFunctor,

      -- Test Applicative
      testApplicativePure,
      testApplicative,

      -- Test Monad
      testMonad,

      -- Test do notation
      testDoNotationEmptyInput,
      testDoNotationSmallInput,
      testDoNotation,
      testDoNotationLargeInput,

      -- Test parsers and combinators
      testSat,
      testSatFail,

      testMany0EmptyInput,
      testMany0Fail,
      testMany0,
      testMany0Extra,

      testMany1EmptyInput,
      testMany1Fail,
      testMany1,
      testMany1Extra,

      testChar,
      testCharFail,

      testString,
      testStringEmptyInput,
      testStringFail,

      testDigit,
      testDigitFail,

      -- Test arithmetic
      testPlus,
      testMinus,
      testMultipy,
      testDivide,
      testAddMultipy,
      testAddDivide,
      testArithmeticLong,
      testArithmeticParentheses,

      -- Test csv
      testCsvFail,
      testCsvString,
      testCsvNumber,
      testCsvNumAsString,
      testRow,
      testRowSpaces,
      testRowFail,
      testRows,
      testRowsFail
    ])
  if (errors counts + failures counts == 0) then
    exitSuccess
  else
    exitFailure
