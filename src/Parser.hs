-- Monadic Parser Combinators

{-

References::

Graham Hutton and Erik Meijer.  (1996) "Monadic Parser Combinators".  Technical
Report, Department of Computer Science, University of Nottingham
(NOTTCS-TR-96-4).

Graham Hutton and Erik Meijer.  (1998) "Monadic Parsing in Haskell".  Journal of
Functional Programming, 8(4):437-444.  https://doi.org/10.1017/S0956796898003050

Graham Hutton.  (2016) _Programming in Haskell_ (Chapter 13: Monadic Parsing).

Text.ParserCombinators.ReadP
https://www.stackage.org/haddock/lts-19.30/base-4.15.1.0/Text-ParserCombinators-ReadP.html

-}

-- ###################################################################
-- ###################################################################

{- ORMOLU_DISABLE -}
module Parser (
  Parser {- instance Functor, Applicative, Alternative, Monad, MonadPlus -},
  parse, parseList, parseMaybe,
  get, peek, look,
  pfail, (+++), (<++), (++>),
  pfilter, satisfy,
  char, string,
  digit, lower, upper, space, letter, alphanum, oneOf, noneOf,
  choice, first,
  manyA, many1A, skipManyA, skipMany1A, sepByA, sepBy1A, chainrA, chainr1A, chainlA, chainl1A,
  manyL, many1L, skipManyL, skipMany1L, sepByL, sepBy1L, chainrL, chainr1L, chainlL, chainl1L,
  skipSpaces,
  between, token, ctoken, stoken,
  ioption, imaybe, xoption, xmaybe,
  count,
  natural, integer
) where
{- ORMOLU_ENABLE -}

import Control.Applicative
import Control.Monad
import Data.Char

-- ###################################################################
-- ###################################################################

-- Core

{-
A parser for things
Is a function from strings
To lists of pairs
Of things and strings
                Graham Hutton
                (w/ apologies to Dr. Seuss)
-}
newtype Parser a = Parser (String -> [(a, String)])

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- ###################################################################

-- Useful (internal and) external function;
-- runs a parser on an input
parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

-- Useful external function;
-- succeeds with all parses of entire input
parseList :: Parser a -> String -> [a]
parseList p s = [x | (x, "") <- parse p s]

-- Useful external function;
-- succeeds if entire input can be parsed in exactly one way.
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p s =
  case parseList p s of
    [x] -> Just x
    _ -> Nothing

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- ###################################################################

--- Primitive actions.

-- Consumes and returns the next character. Fails if there is no input left.
-- Primitive action.
get :: Parser Char
get =
  Parser
    ( \s -> case s of
        [] -> []
        c : cs -> [(c, cs)]
    )

-- Returns the next character (without consuming). Fails if there is no input left.
-- Primitive action.
peek :: Parser Char
peek =
  Parser
    ( \s -> case s of
        [] -> []
        c : _ -> [(c, s)]
    )

-- Look-ahead: returns the part of the input that is left, without consuming it.
-- Primitive action.
look :: Parser String
look = Parser (\s -> [(s, s)])

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- ###################################################################

--- Applicative/Monadic actions

-- Always succeeds; applicative pure / monadic return.
psucceed :: a -> Parser a
psucceed x = Parser (\s -> [(x, s)])

-- (Dependent) sequencing; monadic bind.
pdseq :: forall a b. Parser a -> (a -> Parser b) -> Parser b
pdseq p1 mkP2 =
  Parser
    ( \s0 ->
        let rs1 :: [(a, String)]
            rs1 = parse p1 s0
            p2s :: [(Parser b, String)]
            p2s = [(mkP2 x, s1) | (x, s1) <- rs1]
            rss2 :: [[(b, String)]]
            rss2 = [parse p2 s1 | (p2, s1) <- p2s]
         in concat rss2
    )

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

{-
Equivalent definitions:

pdseq p1 mkP2 =
  Parser
    ( \s0 ->
        let -- parse according to p1
            rs1 :: [(a, String)]
            rs1 = parse p1 s0
            -- for each p1 parse,
            -- parse according to mkP2 applied to
            -- the p1 parse result and the p1 parse remainder
            rss2 :: [[(b, String)]]
            rss2 = map (\(x1, s1) -> parse (mkP2 x1) s1) rs1
            -- flatten list of p2 parses
            rs2 = concat rss2
         in rs2
    )

pdseq p1 f =
  Parser
    ( \s0 ->
        concatMap (\(x, s1) -> parse (f x) s1) (parse p1 s0)
    )

pdseq p1 mkP2 =
  Parser
    ( \s0 ->
        [ (y, s2)
          | (x, s1) <- parse p1 s0,
            (y, s2) <- parse (mkP2 x) s1
        ]
    )

-}

-- Apply a pure function to parse result; functorial mapping.
pfmap :: forall a b. (a -> b) -> Parser a -> Parser b
pfmap f p = Parser (\s0 -> [(f x, s) | (x, s) <- parse p s0])

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

{-
Equivalent definitions:

pfmap f p =
  Parser
    ( \s0 ->
        let -- parse according to p1
            rs1 :: [(a, String)]
            rs1 = parse p s0
            -- for each p1 parse,
            -- apply f to the p1 parse result
            rs2 :: [(b, String)]
            rs2 = map (\(x, s1) -> (f x, s1)) rs1
         in rs2
    )

pfmap f p =
  Parser
    ( \s0 ->
        map (\(x, s1) -> (f x, s1)) (parse p s0)
    )

pfmap f p =
  Parser
    ( \s0 ->
        [(f x, s1) | (x, s1) <- parse p s0]
    )

pfmap f p =
  p `pdseq` \x -> psucceed (f x)

pfmap f p = do
  x <- p
  return (f x)

-}

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) = pdseq

instance Applicative Parser where
  pure :: a -> Parser a
  pure = psucceed
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = do
    f <- pf
    x <- px
    return (f x)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap = pfmap

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- ###################################################################

--- Failure and Choice

-- Always fails.
pfail :: Parser a
pfail = Parser (const [])

infixr 5 +++, <++, ++>

-- Symmetric choice;
-- parse according to *both* p1 and p2.
(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = Parser (\s0 -> parse p1 s0 ++ parse p2 s0)

-- Left-biased choice;
-- parse according to p1, if any successes;
-- parse according to p2, only if p1 fails (produces no successes)
(<++) :: Parser a -> Parser a -> Parser a
p1 <++ p2 =
  Parser
    ( \s0 -> case parse p1 s0 of
        [] -> parse p2 s0
        xss -> xss
    )

-- Right-biased choice;
-- parse according to p2, if any successes;
-- parse according to p1, only if p2 fails (produces no successes)
(++>) :: Parser a -> Parser a -> Parser a
p1 ++> p2 = p2 <++ p1

instance Alternative Parser where
  empty :: Parser a
  empty = pfail
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = p1 +++ p2

instance MonadPlus Parser where
  mzero :: Parser a
  mzero = pfail
  mplus :: Parser a -> Parser a -> Parser a
  p1 `mplus` p2 = p1 +++ p2

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- Derived --

-- Consumes and returns a parsed value, if it satisfies the specified predicate.
pfilter :: (a -> Bool) -> Parser a -> Parser a
pfilter f p =
  p >>= \x ->
    if f x
      then return x
      else mzero

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

{-
Note that `pfilter` is defined only in terms of `(>>=)`, `return`, and `mzero`
(and not in terms of any `Parser`-specific operations).
Thus, `pfilter` can be generalized to any `MonadPlus`.

Not surprisingly, `Control.Monad` provides

  mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a

which is the generalization of `pfilter` to any `MonadPlus`.
-}

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- Consumes and returns the next character, if it satisfies the specified predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = pfilter f get

-- Parses and returns the specified character.
char :: Char -> Parser Char
char c = satisfy (== c)

-- Parses and returns particular kinds of characters
digit :: Parser Char
digit = satisfy isDigit

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

space :: Parser Char
space = satisfy isSpace

letter :: Parser Char
letter = lower +++ upper

alphanum :: Parser Char
alphanum = letter +++ digit

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

noneOf :: [Char] -> Parser Char
noneOf cs = satisfy (not . (`elem` cs))

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- Parses and returns the specified string.
string :: String -> Parser String
string "" = psucceed ""
string (c : cs) = do
  c' <- char c
  cs' <- string cs
  psucceed $ c' : cs'

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

{-
Equivalent definitions:

string []     = return ""
string (c:cs) = do c' <- char c
                   cs' <- string cs
                   return (c':cs')

string s = loop s >> return s
  where
    loop :: String -> Parser ()
    loop [] = return ()
    loop (c : cs) = char c >> loop cs

string s = mapM_ char s >> return s
-}

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- Why are the second and third definitions more efficient?

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- Combines all parsers in the specified list.
choice :: [Parser a] -> Parser a
choice = foldl (+++) pfail

-- Could we have used `foldr`?

-- Parses according to the first succeeding parser in the specified list.
first :: [Parser a] -> Parser a
first = foldl (<++) pfail

-- Could we have used `foldr`?

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- Parses zero or more occurrences of the given parser.
manyA :: Parser a -> Parser [a]
manyA p = return [] +++ many1A p

-- Parses one or more occurrences of the given parser.
many1A :: Parser a -> Parser [a]
many1A p = do
  x <- p
  xs <- manyA p
  return (x : xs)

{-
Equivalent definition
many1A :: Parser a -> Parser [a]
many1A p = (:) <$> p <*> manyA p
-}

-- How many successes can these parsers return?

-- >>> parse (manyA (char 'a')) "aaabbbccc"
-- [("","aaabbbccc"),("a","aabbbccc"),("aa","abbbccc"),("aaa","bbbccc")]

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- Parses zero or more occurrences of the given parser.
manyL :: Parser a -> Parser [a]
manyL p = many1L p <++ return []

-- Parses one or more occurrences of the given parser.
many1L :: Parser a -> Parser [a]
many1L p = do
  x <- p
  xs <- manyL p
  return (x : xs)

{-
Equivalent definition:

many1L :: Parser a -> Parser [a]
many1L p = (:) <$> p <*> manyL p
-}

-- How many successes can these parsers return?

-- >>> parse (manyL (char 'a')) "aaabbbccc"
-- [("aaa","bbbccc")]

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- Why are the `*L` parsers typically preferred to the `*A` parsers?

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- Like `manyA`, but discards the result.
skipManyA :: Parser a -> Parser ()
skipManyA p = void $ manyA p

-- Like `many1A`, but discards the result.
skipMany1A :: Parser a -> Parser ()
skipMany1A p = void $ many1A p

-- `sepByA p sep` parses zero or more occurrences of `p`, separated by `sep`.
-- Returns a list of values returned by `p`.
sepByA :: Parser a -> Parser sep -> Parser [a]
sepByA p sep = return [] +++ sepBy1A p sep

-- `sepBy1A p sep` parses one or more occurrences of `p`, separated by `sep`.
-- Returns a list of values returned by `p`.
sepBy1A :: Parser a -> Parser sep -> Parser [a]
sepBy1A p sep = do
  x <- p
  xs <- manyA (sep >> p)
  return (x : xs)

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- Like `manyL`, but discards the result.
skipManyL :: Parser a -> Parser ()
skipManyL p = void $ manyL p

-- Like `many1L`, but discards the result.
skipMany1L :: Parser a -> Parser ()
skipMany1L p = void $ many1L p

-- `sepByL p sep` parses only the longest (empty or non-empty) occurrences of `p`,
-- separated by `sep`.
-- Returns a list of values returned by `p`.
sepByL :: Parser a -> Parser sep -> Parser [a]
sepByL p sep = sepBy1L p sep <++ return []

-- `sepBy1L p sep` parses only the longest (non-empty) occurrences of `p`,
-- separated by `sep`.
-- Returns a list of values returned by `p`.
sepBy1L :: Parser a -> Parser sep -> Parser [a]
sepBy1L p sep = (:) <$> p <*> manyL (sep >> p)

{-
Equivalent definition:
sepBy1L p sep = do x <- p
                   xs <- manyL (sep >> p)
                   return (x : xs)
-}

{-
Why are the `*L` parsers typically preferred to the `*A` parsers?
-}

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- Skips all whitespace.
skipSpaces :: Parser ()
skipSpaces = skipManyL space

-- `between open close p` parses `open`, followed by `p`, and finally `close`.
-- Only the value of `p` is returned.
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  _ <- open
  x <- p
  _ <- close
  return x

-- `token p` parses spaces before and after `p`.
token :: Parser a -> Parser a
token = between skipSpaces skipSpaces

-- `ctoken c` parses spaces before and after `c`.
ctoken :: Char -> Parser ()
ctoken = void . token . char

-- `stoken s` parses spaces before and after `s`.
stoken :: String -> Parser ()
stoken = void . token . string

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- `ioption x p` will (inclusively) parse `p` or return `x` without consuming any input.
ioption :: a -> Parser a -> Parser a
ioption x p = p +++ return x

-- `imaybe p` will try to parse `p`.
imaybe :: Parser a -> Parser (Maybe a)
imaybe p = ioption Nothing (Just <$> p)

-- `xoption x p` will (exclusively) parse `p` or return `x` without consuming any input.
xoption :: a -> Parser a -> Parser a
xoption x p = p <++ return x

-- `xmaybe p` will try to parse `p`.
xmaybe :: Parser a -> Parser (Maybe a)
xmaybe p = xoption Nothing (Just <$> p)

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- `count n p` parses `n` occurrences of `p` in sequence.
count :: Int -> Parser a -> Parser [a]
count 0 _ = return []
count n p = (:) <$> p <*> count (pred n) p

-- Parse a natural number
natural :: Parser Integer
natural = do
  d <- toInteger . digitToInt <$> digit
  aux d
  where
    aux n =
      ( do
          d <- toInteger . digitToInt <$> digit
          aux (10 * n + d)
      )
        <++ return n

-- >>> parse natural "1234e3 hello"
-- [(1234,"e3 hello")]

-- Parse a (signed) integer
integer :: Parser Integer
integer = ((char '-' >> pure negate) <++ pure id) <*> natural

{-

#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####
#####

-}

-- `chainrA p op x` parses zero or more occurrences of `p`, separated by `op`.
-- Returns a value produced by a *right associative* application
-- of all functions returned by `op`.
-- If there are no occurrences of `p`, `x` is returned.
chainrA :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainrA p op x = return x +++ chainr1A p op

-- Like `chainrA`, but parses one or more occurrences of `p`.
chainr1A :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1A p op = p >>= rest
  where
    rest x =
      return x
        +++ ( do
                f <- op
                y <- p >>= rest
                return (x `f` y)
            )

-- `chainlA p op x` parses zero or more occurrences of `p`, separated by `op`.
-- Returns a value produced by a *left associative* application
-- of all functions returned by `op`.
-- If there are no occurrences of `p`, `x` is returned.
chainlA :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainlA p op x = return x +++ chainl1A p op

-- Like `chainlA`, but parses one or more occurrences of `p`.
chainl1A :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1A p op = p >>= rest
  where
    rest x =
      return x
        +++ ( do
                f <- op
                y <- p
                rest (x `f` y)
            )

-- `chainrL p op x` parses only the longest (empty or non-empty) occurrences of `p`,
-- separated by `op`.
-- Returns a value produced by a *right associative* application
-- of all functions returned by `op`.
-- If there are no occurrences of `p`, `x` is returned.
chainrL :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainrL p op x = chainr1L p op <++ return x

-- Like `chainrL`, but parses only the longest (non-empty) occurrences of `p`.
chainr1L :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1L p op = p >>= rest
  where
    rest x =
      ( do
          f <- op
          y <- p >>= rest
          return (x `f` y)
      )
        <++ return x

-- `chainlL p op x` parses only the longest (empty or non-empty) occurrences of `p`,
-- separated by `op`.
-- Returns a value produced by a *left associative* application
-- of all functions returned by `op`.
-- If there are no occurrences of `p`, `x` is returned.
chainlL :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainlL p op x = chainl1L p op <++ return x

-- Like `chainlL`, but parses only the longest (non-empty) occurrences of `p`.
chainl1L :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1L p op = p >>= rest
  where
    rest x =
      ( do
          f <- op
          y <- p
          rest (x `f` y)
      )
        <++ return x
