{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Imp.Frontend.Lexer.Lexer where

import Control.Monad
}


%wrapper "monadUserState"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

-- second RE macros

@identifier = $alpha[$alpha $digit]* -- identifiers
@number     = $digit+


-- tokens declarations

tokens :-
      -- whitespace and comments 
      <0> $white+       ;
      <0> "//" .*       ;
      <0> "/*"          {nestComment `andBegin` state_comment}
      <0> "*/"          {\ _ _ -> alexError "Error! Unexpected close comment!"}
      <state_comment> "/*"                     {nestComment}
      <state_comment> "*/"                     {unnestComment}
      <state_comment> .                        ;  
      <state_comment> \n                       ;
      
      -- Signs 
      <0> "="         { simpleToken TAssign }
      <0> ":"         { simpleToken TColon }
      <0> "::"        { simpleToken TDoubleColon }
      <0> ","         { simpleToken TComma }
      <0> ";"         { simpleToken TSemi }

      -- Keywords
      <0> "skip"      { simpleToken TSkip }
      <0> "read"      { simpleToken TRead }
      <0> "print"     { simpleToken TPrint }
      <0> "if"        { simpleToken TIf }
      <0> "then"      { simpleToken TThen }
      <0> "else"      { simpleToken TElse }
      <0> "while"     { simpleToken TWhile }
      <0> "function"  { simpleToken TFunction }

      -- Arithmetic operators
      <0> "/"          { simpleToken TDiv }
      <0> "-"          { simpleToken TMinus }
      <0> "*"          { simpleToken TTimes }
      <0> "+"          { simpleToken TPlus }
      <0> "%"          { simpleToken TMod }

      -- Comparison operators
      <0> "=="          { simpleToken TEq }
      <0> "!="          { simpleToken TNotEq }
      <0> "!"           { simpleToken TNot}
      <0> "<"           { simpleToken TLt }
      <0> "<="          { simpleToken TLe }
      <0> ">"           { simpleToken TGt }
      <0> ">="          { simpleToken TGe }

      -- Logical operators
      <0> "&&"          { simpleToken TAnd }
      <0> "||"          { simpleToken TOr }

      -- Parenthesis
      <0> "("           { simpleToken TLParen }
      <0> ")"           { simpleToken TRParen }
      
      -- Brackets
      <0> "["           { simpleToken TLBrack }
      <0> "]"           { simpleToken TRBrack }

      -- Braces
      <0> "{"           {simpleToken TLBrace}
      <0> "}"           {simpleToken TRBrace}
    
      <0> "int"         {simpleToken TTInt}
      <0> "bool"        {simpleToken TTBool}
      <0> "false"       {simpleToken TFalse}

      <0> @number       {mkNumber}
      <0> @identifier   {mkIdent}      
      <0> "true"        {simpleToken TTrue}


  

{

-- user state 

data AlexUserState 
  = AlexUserState {
      nestLevel :: Int 
    }

alexInitUserState :: AlexUserState 
alexInitUserState 
  = AlexUserState 0 

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f 
  = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  startCode <- alexGetStartCode
  when (startCode == state_comment) $
    alexError "Error: unclosed comment"
  pure $ Token (position pos) TEOF

-- token definition

data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme 
    } deriving (Eq, Ord, Show)

data Lexeme    
  = TIdent String

    -- Signs
  | TAssign
  | TColon        
  | TDoubleColon  
  | TComma        
  | TSemi     

  -- Keywords
  | TRead
  | TPrint
  | TIf
  | TThen
  | TElse
  | TWhile  
  | TFunction 

  -- Types
  | TTInt
  | TTBool
  | TString
  | TNumber Int
  | TTrue 
  | TFalse

  -- Arithmetic operators
  | TPlus
  | TMinus
  | TTimes
  | TDiv
  | TMod 

 -- Comparison operators
  | TEq
  | TNotEq
  | TNot
  | TLt
  | TLe
  | TGt
  | TGe

  -- Logical operators
  | TAnd
  | TOr 

  -- Parenthesis
  | TLParen 
  | TRParen 

  -- Brackets
  | TLBrack
  | TRBrack

  -- Braces
  | TLBrace 
  | TRBrace 
   
  | TEOF
  | TSkip
  deriving (Eq, Ord, Show)

position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

mkIdent :: AlexAction Token 
mkIdent (st, _, _, str) len 
  = case take len str of 
      "skip" -> pure $ Token (position st) TSkip
      "if"   -> pure $ Token (position st) TIf
      "then" -> pure $ Token (position st) TThen
      "else" -> pure $ Token (position st) TElse
      "true" -> pure $ Token (position st) TTrue
      "false" -> pure $ Token (position st) TFalse
      "int"   -> pure $ Token (position st) TTInt
      "bool"  -> pure $ Token (position st) TTBool
      "read"  -> pure $ Token (position st) TRead
      "print" -> pure $ Token (position st) TPrint
      "while" -> pure $ Token (position st) TWhile
      _ ->  pure $ Token (position st) (TIdent (take len str))


mkNumber :: AlexAction Token
mkNumber (st, _, _, str) len 
  = pure $ Token (position st) (TNumber $ read $ take len str)


simpleToken :: Lexeme -> AlexAction Token
simpleToken lx (st, _, _, _) _
  = return $ Token (position st) lx

-- dealing with comments

nestComment :: AlexAction Token
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len

unnestComment :: AlexAction Token 
unnestComment input len 
  = do
      s <- get
      let level = (nestLevel s) - 1
      put s{nestLevel = level}
      when (level == 0) $
        alexSetStartCode 0
      skip input len


lexer :: String -> Either String [Token]
lexer s = runAlex s go 
  where 
    go = do 
      output <- alexMonadScan 
      if lexeme output == TEOF then 
        pure [output]
      else (output :) <$> go
}
