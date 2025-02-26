module Frontend.Parser.Recursive.Parser where 
import Frontend.Parser.Recursive.SimpleCombinators
import Frontend.Syntax.Syntax
import Frontend.Lexer.Lexer

-- top level parsing function 

impParser :: String -> Either String Program
impParser s 
  = case runParser defParser s of 
      [] -> Left "Parser error"
      ((r,_) : _) -> Right r

-- main program parser 

-- programParser :: Parser Char Program
-- programParser 
--  = Program <$> blockParser

defParser :: Parser Char Program
defParser 
  = Program <$> many1 def

-- parser for definitions
def :: Parser Char Def
def = dataDefParser <|> functionDefParser


dataDefParser :: Parser Char Def
dataDefParser = DataDef <$> varParser <*> many (braces varDeclParser)
-- parser for statements 

-- function parser
functionDefParser :: Parser Char Def
functionDefParser = f <$> varParser 
                     <*> parens (many1 paramParser)
                     <*> colon 
                     <*> tyParser 
                     <*> many (colon *> tyParser)
                     <*> braces (many stmtParser)
                where 
                  f a b c d e f = Function a b d e f

-- params parser
paramParser :: Parser Char Param
paramParser = f <$> varParser
                    <*> doubleColon 
                    <*> tyParser 
                    <> many (comma *> (Decl <$> varParser <> (doubleColon *> tyParser)))
            where 
              f a _ c d = Param a c d
stmtParser :: Parser Char Stmt
stmtParser 
  = choice [
            stmtsParser 
           , ifParser
           , ifelseParser  
           , iterateParser
           , readParser
           , printParser
           , returnParser
           , assignParser
           , functionCallParser 
           ]

functionCallParser :: Parser Char Stmt
functionCallParser = f <$> varParser                                -- Nome da função
                  <*> parens (commaList expParser)             -- Argumentos entre parênteses
                  <*> angleBrackets (commaList lvalueParser)   -- LValues entre < e >
                  <*> semi                                      -- Ponto e vírgula
  where
    f a b c _ = FunCall a b c

angleBrackets :: Parser Char a -> Parser Char a
angleBrackets p = lessthan > p < morethan

-- lvalue parser
lvalueParser :: Parser Char LValue
lvalueParser = choice [
                       lvarParser
                      ,lexpParser 
                      ]

-- lvar parser
lvarParser :: Parser Char LValue
lvarParser = LVar <$> varParser

lexpParser :: Parser Char LValue
lexpParser = LExp <$> expParser

returnParser :: Parser Char Stmt 
returnParser = f <$> expParser <> many (expParser) <> semi
        where
          f a b _ = Return a b


-- block parser
blockParser :: Parser Char Block
blockParser = Block <$> many stmtParser

-- Stmts parser
stmtsParser:: Parser Char Stmt 
stmtsParser
  = Stmts <$> blockParser

-- parser of iterators
iterateParser :: Parser Char Stmt 
iterateParser = Iterate <$> braces expParser <*> stmtParser

-- variable declaration 

varDeclParser :: Parser Char Decl
varDeclParser
  = f <$> varParser <> doubleColon <> tyParser <*> semi 
    where 
      f t v i _ = Decl t i

-- variable 

varParser :: Parser Char Var 
varParser = Var <$> identifier

-- type 

tyParser :: Parser Char Type 
tyParser = intParser <|> tboolParser 

intParser :: Parser Char Type 
intParser = TInt <$ (stringToken "int")

tboolParser :: Parser Char Type 
tboolParser = TBool <$ (stringToken "bool")

tfloatParser :: Parser Char Type
tfloatParser = TFloat <$ (stringToken "float")

tcharParser :: Parser Char Type 
tcharParser = TChar <$ (stringToken "char")
-- initialization 

initParser :: Parser Char (Maybe Exp)
initParser = (Just <$> rhsParser) <|> pure Nothing 

rhsParser :: Parser Char Exp
rhsParser 
  = f <$> stringToken "=" <*> expParser 
    where
      f _ e = e

-- assignment 

assignParser :: Parser Char Stmt 
assignParser 
  = f <$> varParser <> expParser <> semi 
    where 
      f v e _ = Assign v e

-- if statements
ifParser :: Parser Char Stmt 
ifParser 
  = f <$> stringToken "if"   <*> 
          parens expParser   <*>
          stmtParser 
    where 
      f _ b c = If b c

ifElseParser :: Parser Char Stmt 
ifElseParser 
  = f <$> stringToken "if"   <*> 
          parens expParser   <*>
          stmtParser         <*>
          stringToken "else" <*>
          stmtParser
    where 
      f _ b c _ d = IfElse b c d

-- print statement 
printParser :: Parser Char Stmt 
printParser 
  = Print <$> expParser 

-- read statement 
readParser :: Parser Char Stmt 
readParser 
  = Read <$> lvarParser


-- values 
valueParser :: Parser Char Value 
valueParser = vintParser <|> vboolParser 

vintParser :: Parser Char Value
vintParser = EInt <$> integer 

vfloatParser:: Parser Char Value
vfloatParser = EInt <$> float 

vboolParser :: Parser Char Value 
vboolParser = EBool <$> boolParser 
  where 
    boolParser = trueParser <|> falseParser 
    trueParser = True <$ stringToken "true"
    falseParser = False <$ stringToken "false"

-- expressions.

expParser :: Parser Char Exp 
expParser = andExpParser 

-- and parser
andExpParser :: Parser Char Exp
andExpParser 
  = chainl relExpParser andop 
    where 
      andop = (:&&:) <$ stringToken "&&"

-- or parser
orExpParser :: Parser Char Exp
orExpParser 
  = chainl relExpParser orop 
    where 
      orop = (:||:)<$ stringToken "||"


relExpParser :: Parser Char Exp
relExpParser 
  = chainl plusExpParser relop  
    where 
      relop = eqp <|> nepq <|> ltp <|> leqp <|> gtp <|> geqp
      eqp = (:==:) <$ stringToken "=="
      nepq = (:!=:) <$ stringToken "!="
      ltp = (:<:) <$ stringToken "<"
      leq = (:<=:) <$ stringToken "<="
      gtp = (:>:) <$ stringToken ">"
      geqp = (:<=:) <$ stringToken "<="

plusExpParser :: Parser Char Exp
plusExpParser 
  = chainl mulExpParser plusop  
    where 
      plusop = plusp <|> minusp
      plusp = (:+:) <$ stringToken "+"
      minusp = (:-:) <$ stringToken "-"

mulExpParser :: Parser Char Exp
mulExpParser 
  = chainl notExpParser mulop  
    where 
      mulop = mod <|> timesp <|> divp
      timesp = (::) <$ stringToken ""
      divp = (:/:) <$ stringToken "/"
      mod = (:%:) <$ stringToken "%"

notExpParser :: Parser Char Exp
notExpParser 
  = notTokenList <*> factorParser 

notTokenList :: Parser Char (Exp -> Exp)
notTokenList 
  = foldr step id <$> greedy (stringToken "!")
    where 
      step _ ac = ENot . ac 

-- factorParser :: Parser Char Exp
-- factorParser 
--   = choice [ EValue <$> valueParser
--            , parens expParser 
--            ]
