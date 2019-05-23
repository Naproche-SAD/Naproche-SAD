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
    precedingWhiteSpace :: Bool,
    tokenProper :: Bool} |
  EOF {tokenPos :: SourcePos}

-- TODO: avoid boolean blindness?

--data IsWhitespace = Whitespace | NotWhitespace
--data IsProper = Proper | NotProper



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

tokenize :: SourcePos -> String -> [Token]
tokenize start = tokenizeWith start False

-- The boolean indicates if the token is whitespace or not.
tokenizeWith :: SourcePos -> Bool -> String -> [Token]
tokenizeWith pos ws s
  | not (null lexem) =
      Token lexem pos ws True : tokenizeWith (advancesPos pos lexem) False rest
  where (lexem, rest) = span isLexem s
-- skip whitespace and tell the next token if there was preceding whitespace.
tokenizeWith pos _ws s
  | not (null white) = tokenizeWith (advancesPos pos white) True rest
  where (white, rest) = span isSpace s
tokenizeWith pos ws s@('%':_) =
  Token comment pos False False : tokenizeWith (advancesPos pos comment) ws rest
  where (comment, rest) = break (== '\n') s
tokenizeWith pos ws (c:cs) =
  Token [c] pos ws True : tokenizeWith (advancePos pos c) False cs
tokenizeWith pos _ws [] = [EOF pos]

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
  showsPrec _ (Token s p _ _) = showString s . shows p
  showsPrec _ EOF{} = showString ""
