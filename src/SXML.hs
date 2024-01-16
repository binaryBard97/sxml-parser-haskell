{-

Name: Shreya Patel
Time spent on assignment: 18 hours
Collaborators/Acknowledgements:

-}

module SXML (SXML, sxmlP, sxmlD, main) where

{-
Useful Prelude types and functions.

getContents :: IO String
-}

import Control.Monad (guard, void)
import Data.Char
import Parser
import PrettyPrinter
{-
Useful Stytem.Environment functions.

-- Computation `getArgs` returns a list of the program's command line arguments
-- (not including the program name).
-- Note: When executing a Haskell program as
--   stack runhaskell Prog.hs arg1 arg2 arg3
-- `getArgs` will return `[arg1, arg2, arg3]`.
getArgs :: IO [String]
-}
import System.Environment (getArgs)
{-
Useful Text.Read functions.

-- Parse a string using the `Read` instance. Succeeds if there is exactly one
-- valid result.
readMaybe :: Read a => String -> Maybe a
-}
import Text.Read (readMaybe)
import Valid

-- ###################################################################
-- ###################################################################

infixr 2 |||

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f ||| g = \x -> f x || g x

infixr 3 &&&

(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f &&& g = \x -> f x && g x

-- ###################################################################
-- ###################################################################

newtype SXML = SXML Elt
  deriving (Eq, Read, Show)

instance Valid SXML where
  valid (SXML elt) = valid elt

-- Invariant:: forall (Elt n atts items) . validName n
data Elt = Elt String [Att] [Item]
  deriving (Eq, Read, Show)

instance Valid Elt where
  valid (Elt n atts items) =
    validName n && all valid atts && all valid items

validName :: String -> Bool
validName [] = False
validName (c : cs) = nameStartChar c && all nameChar cs
  where
    nameStartChar = (== ':') ||| (== '_') ||| isAlpha
    nameChar = nameStartChar ||| (== '-') ||| (== '.') ||| isDigit

-- Invariant:: forall (Att n v) . validName n && validAttValue v
data Att = Att String String
  deriving (Eq, Read, Show)

instance Valid Att where
  valid (Att n v) = validName n && validAttValue v

validAttValue :: String -> Bool
validAttValue = all ((/= '<') &&& (/= '>') &&& (/= '"') &&& (not . isSpace ||| (== ' ')))

-- Invariant:: forall (IText s) . validText s
data Item = IElt Elt | IText String
  deriving (Eq, Read, Show)

instance Valid Item where
  valid (IElt elt) = valid elt
  valid (IText s) = validText s

validText :: String -> Bool
validText = (not . null) &&& all ((/= '<') &&& (/= '>') &&& (not . isSpace))

-- ###################################################################

miscP :: Parser ()
miscP = commentP +++ void skipSpaces

commentP :: Parser ()
commentP = void $ between (string "<!--") (string "-->") (manyL (noneOf "-") +++ many1L (char '-' >> noneOf "-"))

eltP :: Parser Elt
eltP = emptyTagEltP +++ startEndTagEltP

emptyTagEltP :: Parser Elt
emptyTagEltP = do
  _ <- char '<'
  name <- nameP
  atts <- manyL (skipSpaces >> attP)
  skipSpaces
  selfClosing <- optionMaybe (string "/>")
  case selfClosing of
    Just _ -> return $ Elt name atts []
    Nothing -> return $ Elt name atts []

startEndTagEltP :: Parser Elt
startEndTagEltP = do
  skipSpaces
  _ <- char '<'
  name <- nameP
  atts <- xmaybe (manyL (skipSpaces >> attP))
  skipSpaces
  _ <- char '>'
  content <- xmaybe (manyL itemP)
  closingTag <-
    optionMaybe
      ( do
          skipSpaces
          _ <- string "</"
          skipSpaces
          _ <- string name
          skipSpaces
          char '>'
      )
  case closingTag of
    Just _ -> return $ Elt name (maybe [] id atts) (maybe [] id content)
    Nothing -> pfail

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = (Just <$> p) <++ pure Nothing

nameP :: Parser String
nameP = do
  firstChar <- satisfy (\c -> c == ':' || c == '_' || isLetter c)
  rest <- manyL (satisfy (\c -> c == ':' || c == '_' || c == '-' || c == '.' || isLetter c || isDigit c))
  return (firstChar : rest)

attP :: Parser Att
attP = do
  name <- nameP
  _ <- optionMaybe (skipSpaces >> char ' ')
  _ <- string "="
  _ <- skipSpaces
  Att name <$> attValueP

attValueP :: Parser String
attValueP = between (char '"') (char '"') $ manyL (noneOf "\"")

itemP :: Parser Item
itemP = eltItemP +++ textItemP

eltItemP :: Parser Item
eltItemP = fmap IElt eltP

textItemP :: Parser Item
textItemP = fmap IText textP

textP :: Parser String
textP = do
  _ <- skipSpaces
  result <- many1L (noneOf " \n\t<>")
  _ <- skipSpaces
  return result

sxmlP :: Parser SXML
sxmlP = do
  miscP
  elt <- eltP
  miscP
  return $ SXML elt

-- ###################################################################

sxmlD :: SXML -> Doc
sxmlD (SXML elt) = eltD elt

eltD :: Elt -> Doc
eltD (Elt n atts []) = text "<" <+> tagD n atts <+> text "/>"
eltD (Elt n atts bodys) = text "<" <+> tagD n atts <+> text ">" <+> itemsD bodys <+> text "</" <+> text n <+> text ">"

tagD :: String -> [Att] -> Doc
tagD n [] = text n
tagD n [att] = text n <@> attD att
tagD n atts = text n <@> group (nest (length n + 2) (fillSep (attD <$> atts)))

attD :: Att -> Doc
attD (Att n v) = text n <+> text "=" <+> text "\"" <+> text v <+> text "\""

itemsD :: [Item] -> Doc
itemsD bodys = group (nest 2 (lbreak <+> pack (itemD <$> bodys)) <+> lbreak)

itemD :: Item -> Doc
itemD (IElt elt) = eltD elt
itemD (IText w) = text w

-- ###################################################################

main :: IO ()
main = do
  let defaultWidth = 80
  args <- getArgs
  let width = case args of
        [] -> defaultWidth
        arg : _ -> case readMaybe arg of
          Just w | w >= 0 -> w
          _ -> defaultWidth
  inp <- getContents
  case parseMaybe sxmlP inp of
    Nothing -> putStrLn "** PARSE ERROR **"
    Just sxml -> pprint width (sxmlD sxml)

-- ###################################################################
-- ###################################################################

{-

> stack runhaskell src/SXML.hs < gettysburg.sxml

> stack runhaskell src/SXML.hs 40 < gettysburg.sxml

> stack run sxmlreformat < gettysburg.sxml

> stack run sxmlreformat 40 < gettysburg.sxml

-}
