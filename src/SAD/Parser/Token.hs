{-
Authors: Andrei Paskevich (2001 - 2008), Steffen Frerix (2017 - 2018)

Tokenization of input.
-}

{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wall #-}

module SAD.Parser.Token
  ( Token (tokenPos, tokenText),
    tokenEndPos,
    tokensRange,
    showToken,
    properToken,
    tokenize,
    tokenReports,
    composeTokens,
    isEOF,
    noTokens)
  where

import Data.Char

import SAD.Core.SourcePos
import qualified SAD.Core.Message as Message
import qualified Isabelle.Markup as Markup


data Token =
  Token {
    tokenText :: String,
    tokenPos :: SourcePos,
    hasPrecedingWhiteSpace :: Bool,
    tokenProper :: Bool} |
  EOF {tokenPos :: SourcePos}

makeToken :: String -> SourcePos -> Bool -> Bool -> Token
makeToken s pos ws proper =
  Token s (rangePos (pos, advancesPos pos s)) ws proper

tokenEndPos :: Token -> SourcePos
tokenEndPos tok@Token{} = advancesPos (tokenPos tok) (tokenText tok)
tokenEndPos tok@EOF {} = tokenPos tok

tokensRange :: [Token] -> SourceRange
tokensRange toks =
  if null toks then noRange
  else makeRange (tokenPos $ head toks, tokenEndPos $ last toks)

showToken :: Token -> String
showToken t@Token{} = tokenText t
showToken EOF{} = "end of input"

properToken :: Token -> Bool
properToken Token {tokenProper} = tokenProper
properToken EOF {} = True

noTokens :: [Token]
noTokens = [EOF noPos]


-- tokenize

tokenize :: SourcePos -> String -> [Token]
tokenize start = posToken start False
  where
    -- The boolean indicates if the token is whitespace or not.
    posToken :: SourcePos -> Bool -> String -> [Token]

    posToken pos ws s
      | not (null lexem) =
          makeToken lexem pos ws True : posToken (advancesPos pos lexem) False rest
      where (lexem, rest) = span isLexem s

    posToken pos _ s
      | not (null white) = posToken (advancesPos pos white) True rest
      where (white, rest) = span isSpace s

    posToken pos ws s@('#':_) =
      makeToken comment pos False False : posToken (advancesPos pos comment) ws rest
      where (comment, rest) = break (== '\n') s

    posToken pos ws (c:cs) =
      makeToken [c] pos ws True : posToken (advancePos pos c) False cs

    posToken pos _ [] = [EOF pos]

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
  let ws = if hasPrecedingWhiteSpace t then " " else ""
  in  ws ++ showToken t ++ composeTokens ts

isEOF :: Token -> Bool
isEOF EOF{} = True
isEOF _     = False




-- Show instances

instance Show Token where
  showsPrec :: Int -> Token -> ShowS
  showsPrec _ (Token s p _ _) = showString s . shows p
  showsPrec _ EOF{} = showString ""
