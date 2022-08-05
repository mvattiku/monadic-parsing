module Lib where

import Control.Applicative
import Control.Monad
import Data.Char

-- type for parsers
newtype Parser a = Parser (String -> [(a, String)])

-- runs the parser on string
parse :: Parser a -> String -> [(a, String)]
parse (Parser p) cs = p cs

{- monand of parsers -}
item :: Parser Char
item = Parser (\cs -> case cs of
                        ""     -> []
                        (c:cs) -> [(c, cs)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser (\cs -> case parse p cs of
                                [] -> []
                                [(a, cs')] -> [(f a, cs')])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = Parser (\cs -> [(a, cs)])
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    f <*> g = Parser (\cs -> case parse f cs of
                                [] -> []
                                [(a, cs')] -> parse (fmap a g) cs')

instance Monad Parser where
    return a = pure a
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser (\cs -> concat [parse (f a) cs' | 
                                (a, cs') <- parse p cs])

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

instance MonadPlus Parser where
    mzero = Parser (\cs -> [])
    mplus p q  = Parser (\cs -> parse p cs ++ parse q cs)


{- do notation -}
p :: Parser (Char, Char)
p = do { c <- item; item; d <- item; return (c,d)}

{- parsers and combinators -}
-- deterministic choice
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (mplus p q) cs of
                            [] -> []
                            (x:xs) -> [x])

-- parse a character that satifies a predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else mzero}

-- zero or more applications of the parser (like regex * matcher)
many0 :: Parser a -> Parser [a]
many0 p = many1 p +++ return []

-- one or more applications of the parser (like regex + matcher)
many1 :: Parser a -> Parser [a]
many1 p = do 
            a <- p
            as <- many0 p
            return (a:as)

-- repeated application of parser seprated by `sep`
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do a <- p
                    as <- many0 (do {sep; p})
                    return (a:as)

-- parse specific character
char :: Char -> Parser Char
char c = sat (c ==)

-- parse specific string
string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op   = do {a <- p; rest a}
                   where
                      rest a = (do f <- op
                                   b <- p
                                   rest (f a b))
                                +++ return a

{- lexical combinators -}
-- parse a string of spaces
space :: Parser String
space = many0 (sat isSpace)

-- parse a symbol (i.e: operators)
symb :: String -> Parser String
symb cs = initToken (token (string cs))

-- parser to throw away trailing spaces
token :: Parser a -> Parser a
token p = do {a <-p; space; return a}

initToken :: Parser a -> Parser a
initToken p = do {space; a <-p; return a}

-- throw away leading spaces and parse string
apply :: Parser a -> String -> [(a,String)]
apply p = parse (do {space; p})

-- parse a string with digits
digit :: Parser Int
digit = do {c <- sat isDigit; return (ord c - ord '0')}

{- example parser for arithmetic expressions -}
expr  :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

expr   = term `chainl1` addop
term   = factor `chainl1` mulop
factor = token digit +++ do {symb "("; n <- expr; symb ")"; return n}

addop  = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop  = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}

{- example parser for csv (comma-separated values) like format -}
data CSV
  = Number Integer
  | Text String
  deriving (Show, Eq)

comma :: Parser Char
comma = initToken( token( char ','))

-- numbers
integer :: Parser Integer
integer = initToken (token (do {d <- many1 (sat isDigit); return (read d)}))

-- strings
csvString :: Parser String
csvString = initToken (initToken (do {char '\''; s <- many0 (sat (/='\'')); char '\''; return s}))

csv :: Parser CSV
csv = do n <- integer; return (Number n)
      `mplus`
      do s <- csvString; return (Text s)

-- parse a single row
row :: Parser [CSV]
row = csv `sepby` comma

-- parse many rows that end with newline
rows :: Parser [[CSV]]
rows = many1 (do r <- row; mnewline; return r)

mnewline :: Parser Char
mnewline = (char '\n')
