
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
	palabras <- getWords "pruebawurf.xml"
--Metodo putStrLn se encuentra dentro del System.IO y dado un dato lo muestra en pantalla
	putStrLn "Proyecto de Lenguajes de Programacion"
	putStrLn "Integrantes: "
	putStrLn " - Jonathan Mendieta"
	putStrLn " - Denisse Pintado"
	putStrLn " - Janina Costa"
	putStrLn "Tema:"
	putStrLn " - Parseo XML"
	putStrLn ""
	limpiador "respuesta.txt" palabras --Guarda en un archivo aparte

--funcion que recoge una linea de codigo 
limpiador respuesta [] = return ()
limpiador respuesta (x:xs) = do 
	let nuevo = splitOneOf("<>=\"?") x 
	printMe respuesta nuevo
	limpiador respuesta xs
	
---------tags del documento identificados
data Device = Device { id_device :: String, 
                       user_agent :: String, 
                       fall_back :: String
                     } deriving (Eq,Show,Read)
                                         
data Group = Group { id_group :: String
                   } deriving (Eq,Show,Read)

data Capability = Capability { name :: String,
                               value :: String
                             } deriving (Eq,Show,Read)
	
type Atributo = (String,String)
	
	
imprimirTag :: Device -> String
imprimirTag(Device id user fall)="***DEVICE*** "++"\n"++id++" Atributo:User_Agent: "++user++"Atributo:Fall_Back: "++fall
		   			  


------Funcion guarda un documento text aparte y muestra por pantalla el resultado
printMe respuesta [] = return ()   
printMe respuesta (x:xs) = do	
			if x == " " then return()
			else 
				if x == "id_device"  then do
						let sentencia = "****************************************DEVICE***************************************"++"\n" ++"Group:"++ x ++ "::: "
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if isInfixOf "user_agent" x then do
						let sentencia = " Device:" ++ x ++":::"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if isInfixOf "fall_back" x then do
						let sentencia = "Device: " ++ x ++ ":::"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if x == "id"  then do
						let sentencia = "**------------------------------------GROUP-------------------------------**" ++"\n" ++"Group:"++ x ++ "::: "
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if x =="name"  then do
						let sentencia =  "**---------------------------CAPABILITY-------------------**" ++"\n" ++"Capability:"++ x ++ "::: "
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if x =="value"  then do
						let sentencia = "Capability:" ++ x ++":::"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if x == "group" then do
						let sentencia = "******TagGroup" 
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if x == "/" then do
						let sentencia = "---CierreTagCapability---" 
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if x == "/group" then do
						let sentencia = "---CierreTagGroup---" 
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if x == "/device" then do
						let sentencia = "---CierreTagDevice---" 
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if x == "/devices" then do
						let sentencia = "---CierreTagDevices---" 
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if x == "/" then do
						let sentencia = "---CierreTag---" 
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if x == "device"  then do
						let sentencia = "********TagDevice" 
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if x == "devices"  then do
						let sentencia = "**********TagDevices"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if x == "capability"  then do
						let sentencia = "****TagCapability"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if x == "xml"  then do
						let sentencia = "Cabecera_del_Documento_XML"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
			    else if x == "version"  then do
						let sentencia = " "++x++"::::"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else if x == "encoding"  then do
						let sentencia = " "++x++"::::"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs
				else do
						let sentencia = ">>" ++ x ++ " "++"\n"
						putStrLn sentencia
						appendFile respuesta sentencia
						printMe respuesta xs

						

		
	
