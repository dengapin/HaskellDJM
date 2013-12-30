import Text.Parsec
import Text.Parsec.String

data CInstruction = Increment | Decrement
                    deriving (Show)

					
parseIncrement, parseDecrement:: Parser CInstruction

parseGen :: Char -> CInstruction -> Parser CInstruction
parseGen x y = char x >> return y

parseIncrement = parseGen '+' Increment
parseDecrement = parseGen '-' Decrement


   
parseComment :: Parser ()
parseComment = do
  many $ noneOf "<>"
  return ()
  
parseInstruction :: Parser CInstruction
parseInstruction = do
  parseComment
  i <- parseIncrement <|> parseDecrement
  parseComment
  return i
  
parseInstructions :: Parser [CInstruction]
parseInstructions = many parseInstruction
					
main :: IO ()
main = do
  cont <- readFile "ejemplo.xml"
  case parse parseInstructions "ejemplo.xml" cont of
    Left e -> print e
    Right insn -> print insn
