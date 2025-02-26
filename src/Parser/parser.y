{
{-# LANGUAGE DeriveFoldable #-}
module Parser
  ( parseMiniML
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Scientific (Scientific)

import qualified Lexer as L
}

%name parseMiniML decs
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }
%expect 0

%token
  -- Identifiers
  identifier { L.RangedToken (L.Identifier _) _ }
  -- Constants
  string     { L.RangedToken (L.String _) _ }
  number     { L.RangedToken (L.Number _) _ }
  -- Keywords
  let        { L.RangedToken L.Let _ }
  if         { L.RangedToken L.If _ }
  then       { L.RangedToken L.Then _ }
  else       { L.RangedToken L.Else _ }
  -- Arithmetic operators
  '+'        { L.RangedToken L.Plus _ }
  '-'        { L.RangedToken L.Minus _ }
  '*'        { L.RangedToken L.Times _ }
  '/'        { L.RangedToken L.Divide _ }
  -- Comparison operators
  '='        { L.RangedToken L.Eq _ }
  '!='       { L.RangedToken L.Neq _ }
  '<'        { L.RangedToken L.Lt _ }
  '<='       { L.RangedToken L.Le _ }
  '>'        { L.RangedToken L.Gt _ }
  '>='       { L.RangedToken L.Ge _ }
  -- Logical operators
  '&&'        { L.RangedToken L.And _ }
  '||'        { L.RangedToken L.Or _ }
  -- Parenthesis
  '('        { L.RangedToken L.LPar _ }
  ')'        { L.RangedToken L.RPar _ }
  -- Lists
  '['        { L.RangedToken L.LBrack _ }
  ']'        { L.RangedToken L.RBrack _ }
  ','        { L.RangedToken L.Comma _ }
  -- Types
  ':'        { L.RangedToken L.Colon _ }
  '::'        { L.RangedToken L.DoubleColon _ }
  '->'       { L.RangedToken L.Arrow _ }
  -- Projection
  '.'        { L.RangedToken L.Dot _ }
  -- Pattern matching
  match      { L.RangedToken L.Match _ }
  with       { L.RangedToken L.With _ }

%right else in
%right '->'
%left '||'
%left '&&'
%nonassoc '=' '!=' '>' '<=' '>='
%left '+' '-'
%left '*' '/'

%%

optional(p)
  :   { Nothing }
  | p { Just $1 }

many_rev(p)
  :               { [] }
  | many_rev(p) p { $2 : $1 }

many(p)
  : many_rev(p) { reverse $1 }

sepBy_rev(p, sep)
  :                         { [] }
  | sepBy_rev(p, sep) sep p { $3 : $1 }

sepBy(p, sep)
  : sepBy_rev(p, sep) { reverse $1 }

name :: { Name L.Range }
  : identifier { unTok $1 (\range (L.Identifier name) -> Name range name) }

type :: { Type L.Range }
  : name           { TVar (info $1) $1 }
  | '(' ')'        { TUnit (L.rtRange $1 <-> L.rtRange $2) }
  | '(' type ')'   { TPar (L.rtRange $1 <-> L.rtRange $3) $2 }
  | '[' type ']'   { TList (L.rtRange $1 <-> L.rtRange $3) $2 }
  | type '->' type { TArrow (info $1 <-> info $3) $1 $3 }

typeAnnotation :: { Type L.Range }
  : ':' type { $2 }

dec :: { Dec L.Range }
  : let name many(pat) optional(typeAnnotation) '=' exp { Dec (L.rtRange $1 <-> info $6) $2 $3 $4 $6 }

decs :: { [Dec L.Range] }
  : many(dec) { $1 }

exp :: { Exp L.Range }
  : expapp                   { $1 }
  | expcond                  { $1 }
  | '-' exp                  { ENeg (L.rtRange $1 <-> info $2) $2 }
  -- Arithmetic operators
  | exp '+'  exp             { EBinOp (info $1 <-> info $3) $1 (Plus (L.rtRange $2)) $3 }
  | exp '-'  exp             { EBinOp (info $1 <-> info $3) $1 (Minus (L.rtRange $2)) $3 }
  | exp '*'  exp             { EBinOp (info $1 <-> info $3) $1 (Times (L.rtRange $2)) $3 }
  | exp '/'  exp             { EBinOp (info $1 <-> info $3) $1 (Divide (L.rtRange $2)) $3 }
  -- Comparison operators
  | exp '='  exp             { EBinOp (info $1 <-> info $3) $1 (Eq (L.rtRange $2)) $3 }
  | exp '!=' exp             { EBinOp (info $1 <-> info $3) $1 (Neq (L.rtRange $2)) $3 }
  | exp '<'  exp             { EBinOp (info $1 <-> info $3) $1 (Lt (L.rtRange $2)) $3 }
  | exp '<=' exp             { EBinOp (info $1 <-> info $3) $1 (Le (L.rtRange $2)) $3 }
  | exp '>'  exp             { EBinOp (info $1 <-> info $3) $1 (Gt (L.rtRange $2)) $3 }
  | exp '>=' exp             { EBinOp (info $1 <-> info $3) $1 (Ge (L.rtRange $2)) $3 }
  -- Logical operators
  | exp '&&' exp             { EBinOp (info $1 <-> info $3) $1 (And (L.rtRange $2)) $3 }
  | exp '||' exp             { EBinOp (info $1 <-> info $3) $1 (Or (L.rtRange $2)) $3 }
  | dec in exp               { ELetIn (info $1 <-> info $3) $1 $3 }
  -- Exercise 3 bonus:
  -- Note: Suppose we have this:
  -- match 0 with | 0 -> match 1 with | 1 -> 1 | 2 -> 2
  -- Shifting will cause it to parse like so:
  -- match 0 with | 0 -> (match 1 with | 1 -> 1 | 2 -> 2)
  -- Reducing would give:
  -- (match 0 with | 0 -> match 1 with | 1 -> 1) | 2 -> 2

case :: { Case L.Range }
  : '||' pat '->' exp { Case (L.rtRange $1 <-> info $4) $2 $4 }

expapp :: { Exp L.Range }
  : expapp projatom          { EApp (info $1 <-> info $2) $1 $2 }
  | projatom                 { $1 }

expcond :: { Exp L.Range }
  : if exp then exp %shift   { EIfThen (L.rtRange $1 <-> info $4) $2 $4 }
  | if exp then exp else exp { EIfThenElse (L.rtRange $1 <-> info $6) $2 $4 $6 }

-- Exercise 2:
projatom :: { Exp L.Range }
  : atom many(projection)
    { maybe $1 (\projs -> EProj (info $1 <-> info (NE.last projs)) $1 projs) (NE.nonEmpty $2) }

atom :: { Exp L.Range }
  : number                   { unTok $1 (\range (L.Number num) -> ENumber range num) }
  | name                     { EVar (info $1) $1 }
  | string                   { unTok $1 (\range (L.String string) -> EString range string) }
  | '(' ')'                  { EUnit (L.rtRange $1 <-> L.rtRange $2) }
  | '[' sepBy(exp, ',') ']'  { EList (L.rtRange $1 <-> L.rtRange $3) $2 }
  | '(' exp ')'              { EPar (L.rtRange $1 <-> L.rtRange $3) $2 }
    -- Arithmetic operators
  | '(' '+' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Plus (L.rtRange $2)) }
  | '(' '-' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Minus (L.rtRange $2)) }
  | '(' '*' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Times (L.rtRange $2)) }
  | '(' '/' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Divide (L.rtRange $2)) }
  -- Comparison operators
  | '(' '=' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Eq (L.rtRange $2)) }
  | '(' '!=' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (Neq (L.rtRange $2)) }
  | '(' '<' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Lt (L.rtRange $2)) }
  | '(' '<=' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (Le (L.rtRange $2)) }
  | '(' '>' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Gt (L.rtRange $2)) }
  | '(' '>=' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (Ge (L.rtRange $2)) }
  -- Logical operators
  | '(' '&&' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (And (L.rtRange $2)) }
  | '(' '||' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Or (L.rtRange $2)) }

-- Exercise 3:
pat :: { Pat L.Range }
  : number                     { unTok $1 (\range (L.Number num) -> PNumber range num) }
  | name                       { PVar (info $1) $1 }
  | string                     { unTok $1 (\range (L.String string) -> PString range string) }
  | '(' ')'                    { PUnit (L.rtRange $1 <-> L.rtRange $2) }
  | '(' pat ')'                { PPar (L.rtRange $1 <-> L.rtRange $3) $2 }
  | '(' pat typeAnnotation ')' { PAnnot (L.rtRange $1 <-> L.rtRange $4) $2 $3 }
  | '[' sepBy(pat, ',') ']'    { PList (L.rtRange $1 <-> L.rtRange $3) $2 }

projection :: { Proj L.Range }
  : '.' '(' exp ')'          { ProjList (L.rtRange $1 <-> L.rtRange $4) $3 }

{
parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

-- | Build a simple node by extracting its token type and range.
unTok :: L.RangedToken -> (L.Range -> L.Token -> a) -> a
unTok (L.RangedToken tok range) ctor = ctor range tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the
-- second range.
-- Invariant: The LHS range starts before the RHS range.
(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = L.Range a1 b2

-- * AST

data Name a
  = Name a ByteString
  deriving (Foldable, Show)

data Type a
  = TVar a (Name a)
  | TPar a (Type a)
  | TUnit a
  | TList a (Type a)
  | TArrow a (Type a) (Type a)
  deriving (Foldable, Show)

data Dec a
  = Dec a (Name a) [Pat a] (Maybe (Type a)) (Exp a)
  deriving (Foldable, Show)

data Operator a
  = Plus a
  | Minus a
  | Times a
  | Divide a
  | Eq a
  | Neq a
  | Lt a
  | Le a
  | Gt a
  | Ge a
  | And a
  | Or a
  deriving (Foldable, Show)

data Proj a
  = ProjList a (Exp a)
  deriving (Foldable, Show)

data Case a
  = Case a (Pat a) (Exp a)
  deriving (Foldable, Show)

data Exp a
  = ENumber a Scientific
  | EVar a (Name a)
  | EString a ByteString
  | EUnit a
  | EList a [Exp a]
  | EPar a (Exp a)
  | EApp a (Exp a) (Exp a)
  | EIfThen a (Exp a) (Exp a)
  | EIfThenElse a (Exp a) (Exp a) (Exp a)
  | ENeg a (Exp a)
  | EBinOp a (Exp a) (Operator a) (Exp a)
  | EOp a (Operator a)
  | ELetIn a (Dec a) (Exp a)
  | EProj a (Exp a) (NonEmpty (Proj a))
  | EMatch a (Exp a) [Case a]
  deriving (Foldable, Show)

data Pat a
  = PNumber a Scientific
  | PVar a (Name a)
  | PString a ByteString
  | PUnit a
  | PPar a (Pat a)
  | PAnnot a (Pat a) (Type a)
  | PList a [Pat a]
  deriving (Foldable, Show)
}
