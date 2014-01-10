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
	let nuevo = split (oneOf ";{[+#:%&*}]") x 
	printMe respuesta nuevo
	limpiador respuesta xs
printMe respuesta [] = return ()   
printMe respuesta (x:xs) = do	
			if x == "" then return ()
			else 
				if isInfixOf "<?" x then do
						let sentencia = "Cabecera," ++ x ++"\n"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if isInfixOf "<" x then do
						let sentencia = "TagOpen," ++ x ++"\n"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if isInfixOf ">" x then do
						let sentencia = "TagClose," ++ x ++"\n"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else do
						let sentencia = "Expresion, " ++ x ++ "\n"
						appendFile respuesta sentencia
