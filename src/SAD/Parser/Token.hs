{-
Authors: Andrei Paskevich (2001 - 2008), Steffen Frerix (2017 - 2018)

Tokenization of input.
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wall #-}

module SAD.Parser.Token
  ( Token (tokenPos, tokenText),
    tokenEndPos,
    tokensRange,
    showToken,
    properToken,
    mathToken,
    tokenize,
    tokenReports,
    composeTokens,
    isEOF,
    noTokens)
  where

import Data.Char (isSpace, isAscii, isAlphaNum)

import SAD.Core.SourcePos
import qualified SAD.Core.Message as Message
import qualified Isabelle.Markup as Markup


data Token =
  Token {
    tokenText :: String,
    tokenPos :: SourcePos,
    precedingWhiteSpace :: Bool,
    tokenProper :: Bool,
    mathMode :: Bool } |
  EOF {tokenPos :: SourcePos}


tokenEndPos :: Token -> SourcePos
tokenEndPos tok@Token{} = advancesPos (tokenPos tok) (tokenText tok)
tokenEndPos tok@EOF {} = tokenPos tok

tokensRange :: [Token] -> SourceRange
tokensRange toks =
  if null toks then noRange
  else makeRange (tokenPos $ head toks, tokenEndPos $ last toks)

showToken :: Token -> String
showToken t@Token{} = tokenText t
showToken EOF{} = "EOF"

properToken :: Token -> Bool
properToken Token {tokenProper} = tokenProper
properToken EOF {} = True

mathToken :: Token -> Bool
mathToken Token {mathMode} = mathMode
mathToken EOF {} = False

noTokens :: [Token]
noTokens = [EOF noPos]

tokenize :: SourcePos -> String -> [Token]
tokenize start = tokenizeWith start False False

-- The first boolean indicates if the token is whitespace or not.
-- The second boolean indicates if the token is within math mode.
tokenizeWith :: SourcePos -> Bool -> Bool -> String -> [Token]

-- This branch succeeds if the first character is a valid word lexeme character.
tokenizeWith pos ws math s | not (null lexem) = tok : toks
    where
      tok  = Token lexem pos ws True math
      toks = tokenizeWith (advancesPos pos lexem) False math rest
      (lexem, rest) = span isLexem s

-- Skip whitespace and tell the next token that there was preceding whitespace.
tokenizeWith pos _ws math s | not (null white) = toks
  where
    toks = tokenizeWith (advancesPos pos white) True math rest
    (white, rest) = span isSpace s

-- Toggle math mode.
tokenizeWith pos ws math ('$':rest) = toks
  where
    toks = tokenizeWith (advancePos pos '$') ws (not math) rest

-- Parse a comment, which yields a non-proper Token.
tokenizeWith pos ws math s@('%':_) = tok : toks
  where
    tok  = Token comment pos False False False
    toks = tokenizeWith (advancesPos pos comment) ws math rest
    (comment, rest) = break (== '\n') s

-- All other characters are tokenized as one-character symbols.
tokenizeWith pos ws math (c:cs) = tok : toks
  where
    tok  = Token [c] pos ws True math
    toks = tokenizeWith (advancePos pos c) False math cs

-- Then end of the string is marked with an EOF token.
tokenizeWith pos _ws _math [] = [EOF pos]

isLexem :: Char -> Bool
isLexem c =
  isAscii c && isAlphaNum c ||
  c `elem` ['α'..'ω'] ||
  c `elem` ['Α'..'Ω'] ||
  c `elem` ['ℵ'..'ℸ'] ||
  c == '_'

-- markup reports

tokenReports :: Token -> [Message.Report]
tokenReports Token {tokenPos = pos, tokenProper} =
  if tokenProper then []
  else [(pos, Markup.comment1)]
tokenReports _ = []


-- useful functions

composeTokens :: [Token] -> String
composeTokens [] = ""
composeTokens (t:ts) =
  let ws = if precedingWhiteSpace t then " " else ""
  in  ws ++ showToken t ++ composeTokens ts

isEOF :: Token -> Bool
isEOF EOF{} = True
isEOF _     = False




-- Show instances

instance Show Token where
  showsPrec :: Int -> Token -> ShowS
  showsPrec _ tok = case tok of
    Token s pos _white _proper _math -> showString s . shows pos
    EOF{} -> showString ""
