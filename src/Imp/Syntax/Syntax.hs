module Frontend.Syntax.Syntax where

import Data.List

-- definition of expressions

newtype Var = Var { unVar :: String } deriving (Eq, Ord, Show)

newtype Program = Program [Def]
    deriving (Eq, Show)

data Def
    = DataDef Var [Decl]  -- Definição de data com nome e declarações
    | Function Var [Param] Type [Type] [Stmt]  -- Função com nome, parâmetros, tipo(s) de retorno, e comandos
    deriving (Eq, Show)


data Decl = Decl Var Type
    deriving (Eq, Show)


data Param = Param Var Type [Decl]
    deriving (Eq, Show)

data Type 
    = TInt 
    | TChar 
    | TBool 
    | TFloat 
    deriving (Eq, Show)

data Block = Block [Stmt]
    deriving (Eq, Show)

data Stmt 
    = Stmts Block
    -- If statements
    | If Exp Stmt
    | IfElse Exp Stmt Stmt  
    | Iterate Exp Stmt 
    | Read LValue              
    | Print Exp                
    | Return Exp [Exp]        
    | LAssign LValue Exp 
    | Assign Var Exp 
    | FunCall Var [Exp] [LValue] 
    deriving (Eq, Show)

data Exp
    -- Operators
    = Exp :+: Exp              
    | Exp :-: Exp              
    | Exp :*: Exp              
    | Exp :/: Exp              
    | Exp :%: Exp              
    
    -- Comparators
    | Exp :<: Exp              
    | Exp :>: Exp              
    | Exp :<=: Exp              
    | Exp :>=: Exp              
    | Exp :==: Exp             
    | Exp :!=: Exp            
    | Exp :&&: Exp             
    | Exp :||: Exp             

    -- Neg & not
    | ENot Exp
    | ENeg Exp
    | EValue Value
    | EVal LValue
    | FunctionExp Var [Exp][Exp]
    deriving (Eq, Show)

data LValue
    = LVar Var
    | LExp Exp               
    deriving (Eq, Show)

data Exps = Exps [Exp]
    deriving (Eq, Show)

data Value
    = EInt Int
    | EFloat Float
    | EBool Bool
    | EChar Char
    | ENull
    | ENew Type [Exp]
    | EFunction String [Param] [Type] [Stmt]
    deriving (Eq, Show)
