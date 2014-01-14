
import Text.ParserCombinators.Parsec
import Text.Parsec
import Text.Parsec.String


import System.IO
import Data.Char(toUpper)
import Data.List
import Data.List.Split

{-funcion que crea una lista con todas las palabras  a partir de un archivo-}
getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (words contents)	
{-Funcion Main del Proyecto-}
main = do 
{-Funcion getWord recoge un archivo y retorna IOSTRING []-}
	palabras <- getWords "ejemplo.xml"
--Metodo putStrLn se encuentra dentro del System.IO y dado un dato lo muestra en pantalla
	putStrLn "Proyecto de Lenguajes de Programacion"
	putStrLn "Integrantes: "
	putStrLn " - Jonathan Mendieta"
	putStrLn " - Denisse Pintado"
	putStrLn " - Janina Costa"
	putStrLn "Tema:"
	putStrLn " - Parseo XML"
	putStrLn ""
	limpiador "respuesta.txt" palabras

--funcion que recoge una linea de codigo 
limpiador respuesta [] = return ()
limpiador respuesta (x:xs) = do 
	let nuevo = splitOneOf("=<>/;{[+#:%&*}]\\\"") x 
	printMe respuesta nuevo
	limpiador respuesta xs
	
printMe respuesta [] = return ()   
printMe respuesta (x:xs) = do	
			if x == "" then return ()
			else 
				if isInfixOf "id_device" x then do
						let sentencia = "Device," ++ x ++"\n"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if isInfixOf "user_agent" x then do
						let sentencia = "Device," ++ x ++"\n"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if isInfixOf "fallback" x then do
						let sentencia = "Device," ++ x ++"\n"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if isInfixOf "id" x then do
						let sentencia = "Group," ++ x ++"\n"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if isInfixOf "name" x then do
						let sentencia = "Capability," ++ x ++"\n"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if isInfixOf "value" x then do
						let sentencia = "Capability," ++ x ++"\n"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else do
						let sentencia = "Expresion, " ++ x ++ "\n"
						appendFile respuesta sentencia
						

		   			  
