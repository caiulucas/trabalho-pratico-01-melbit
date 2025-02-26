import Imp.Interpreter.ImpInterpreter
import Imp.Frontend.Lexer.Lexer (lexer)
import qualified Imp.Frontend.Parser.LALR.Parser as L
import qualified Imp.Frontend.Parser.Recursive.Parser as R
import qualified Imp.Frontend.Parser.PEG.Parser as P 
import Imp.Frontend.Semantics.StatementTypeChecker
import Imp.Syntax.Syntax

import Control.Monad 
import System.Environment
import System.FilePath

-- main function for Imp 

main :: IO ()
main = do 
  opts <- parseOptions 
  either putStrLn runWithOptions opts 

-- running according to options 

runWithOptions :: Option -> IO ()
runWithOptions opts 
  = do 
      content <- readFile (file opts)
      case parserType opts of
        Lex -> do
          let tokens = lexer content
          print tokens
        _ -> do
          let tree = parseWithOptions opts content 
          case backEnd opts of 
            Left Typed ->
              case tree of 
                Left err -> print err 
                Right ast -> 
                  case tcProgram ast of 
                    Left err -> print err 
                    Right _  -> void $ interpProgram ast 
            Left Untyped -> 
              case tree of 
                Left err -> print err  
                Right ast -> void $ interpProgram ast 

parseWithOptions :: Option -> String -> Either String Program
parseWithOptions opts content
  = case parserType opts of
      LALR -> L.impParser content  
      Recursive -> R.impParser content  
      PEG -> P.impParser content
      Lex -> Left "Lexer does not produce an AST. Use --lexer to view tokens."

-- data type for command line options

data Option 
  = Option {
      parserType :: ParserType   
    , backEnd :: Either Interpreter CodeGen 
    , file :: FilePath
    } deriving Show 

emptyOption :: Option 
emptyOption = Option LALR (Left Untyped) ""

-- flag which determine, what should be executed.

data ParserType 
  = LALR 
  | Lex
  | Recursive 
  | PEG 
  deriving Show 

data Interpreter 
  = Typed 
  | Untyped
  deriving Show 

data CodeGen 
  = SVMCompiler
  | CCompiler
  deriving Show 

-- error message, when parameters are passed wrong

errorMessage :: String
errorMessage 
  = unlines ("Invalid parameter usage!" : helpMessage)


helpMessage :: [String]
helpMessage = [ "Imp - compiler"
              , "Usage:"
              , "lang <flags> <file>"
              , "<flag> : parser / execution options"
              , "* Parser options:"
              , "--lexer: show lexer tokens created"
              , "--recursive: use recursive descendent parser"
              ]

buildOption :: String -> Either String Option -> Either String Option
buildOption _ (Left err) = Left err
buildOption fg (Right opt) 
  = case fg of 
      "--lexer" -> Right (opt{parserType = Lex})
      "--recursive" -> Right (opt{parserType = Recursive})
      "--help" -> Left $ unlines helpMessage
      _ -> Left errorMessage  

parseOptions :: IO (Either String Option)
parseOptions = do
  args <- getArgs
  case reverse args of
    [] -> pure $ Left errorMessage
    (file:flags) -> 
      let initialOption = Right (emptyOption {file = file})
      in pure $ foldr buildOption initialOption flags
