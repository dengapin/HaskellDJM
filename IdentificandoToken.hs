import Text.ParserCombinators.Parsec
import Text.Parsec
import Text.Parsec.String
import System.IO
import Data.Char(toUpper)
import Data.List
import Data.List.Split
import System.IO
import System.Exit
import Debug.Trace

{-Funcion que crea una lista con todas las palabras  a partir de un archivo-}
getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (words contents)   

				   
{-Funcion Main del Proyecto-}
main = do 
{-Funcion getWord recoge un archivo y retorna IOSTRING []-}
	palabras <- getWords "ejemplo.xml"
--Metodo putStrLn se encuentra dentro del System.IO y dado un dato lo muestra en pantalla
	putStrLn "Proyecto de Lenguajes de Programacion"
	putStrLn "Integrante: "
	putStrLn " - Denisse Pintado"
	putStrLn "Tema:"
	putStrLn " - Parseo XML"
	limpiador "respuesta.txt" palabras 
	contentFile <- readFile "wurf.xml"
	archivo <- lecturaLinea contentFile
	tomarConsulta archivo
			
		
lecturaLinea :: String -> IO [String]
lecturaLinea cadenaArchivo = return (lines cadenaArchivo)

--funcion que recoge una linea de codigo 
limpiador respuesta [] = return ()
limpiador respuesta (x:xs) = do 
        let nuevo = splitOneOf("<>=\"?") x 
        printMe respuesta nuevo
        limpiador respuesta xs
		
quitarEspacios ::[String]->[String]
quitarEspacios [] = []
quitarEspacios (x:xs)=do
	if x==""
	then []++quitarEspacios xs
	else [x]++quitarEspacios xs
        
---------Tags del documento xml identificados----------
--------------------Device-----------------------------
data Device = Device { id_device :: String, 
                       user_agent :: String, 
                       fall_back :: String
                     } deriving (Eq,Show,Read)
			
tagDevice :: [String]->[String]
tagDevice [] = []
tagDevice all@(x:xs) = do
	if x=="id"
	then [head xs]++tagDevice xs
	else if x == "user_agent"
		then [head xs]++tagDevice xs
		else if x == "fall_back"
			then [head xs]++tagDevice xs
			else tagDevice xs
					 
atributoDevice ::[String] -> Device
atributoDevice [] = Device "" "" ""
atributoDevice all@(a:b:cs) = do
	Device a b (head cs)

idDevice :: Device -> String
idDevice (Device id user fall) = id

userDevice :: Device -> String
userDevice (Device id user fall) = user

fallDevice :: Device -> String
fallDevice (Device id user fall) = fall					 
--------------------Group----------------------------
                                         
data Group = Group { id :: String
                   } deriving (Eq,Show,Read)
				   


tagGroup :: [String]->[String]
tagGroup [] = []
tagGroup all@(x:xs) = do
	if x=="id"
	then [head xs]++tagGroup xs
	else tagGroup xs

				   
atributoGroup ::[String] -> Group
atributoGroup [] = Group ""
atributoGroup lista = do
	Group (head lista)

idGroup :: Group -> String
idGroup (Group id) = id

--------------------Capability----------------------------

data Capability = Capability { name :: String,
                               value :: String
                             } deriving (Eq,Show,Read)
							 

tagCapability :: [String]->[String]
tagCapability [] = []
tagCapability all@(x:xs) = do
	if x=="name"
	then [head xs]++tagCapability xs
	else if x == "value"
		then [head xs]++tagCapability xs
		else tagCapability xs

atributoCapability ::[String] -> Capability
atributoCapability [] = Capability "" ""
atributoCapability all@(x:xs) = do
	Capability x (head xs)

nameCapabi :: Capability -> String
nameCapabi (Capability name value) = name

valueCapabi :: Capability -> String
valueCapabi (Capability name value) = value
------Funcion guarda un documento text aparte y muestra por pantalla el resultado---------------------------------
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
												
												
-------------------------------------------------CONSULTAS--------------------------------------------------------------------------
cont ::Int->Int->Int
cont  i j = i+j

consultar ::[String] ->String-> IO()
consultar [] _ = putStrLn "\t>>>>>>>>No existen coincidencias en el archivo xml<<<<<<<<<<"
consultar list fallback= do
	putStrLn "\n\t>>>>tIngrese (1) para saber cuantos y (2) para saber cuales<<<<<<<"
	putStr "\t\tIngese el numero: "
	number <- getLine
	if number == "1"
	then putStrLn $ ">>>>Existen "++show(length list) ++" Device"
	else if  number == "2"
		then putStrLn $ ">>>>>Son :"++show(fallback)++"\n"++show( list)
		else if number == "3"
			then exitSuccess
			else consultar list fallback
			
------------------------------Sustentacion

tomarConsulta :: [String] -> IO ()
tomarConsulta [] = return ()
tomarConsulta (x:xs) = do
				if isInfixOf "<devices" x 
				then do
						putStr "\nIngrese fallback de device  buscar: "
						fallback <- getLine
						let listD = listaDevice fallback
						let number = length listD
						let listNew = quitarEspacios xs
						--let list = listaDevice listNew listD (Group "") (Capability "" "")   number number
						consultar listD fallback
				else
					tomarConsulta xs

listaCapability :: String -> [String]
listaCapability [] = []
listaCapability cadena = quitarEspacios( splitOneOf(",; \"") cadena)


listaDevice :: String -> [String]
listaDevice [] = []
listaDevice cadena = quitarEspacios( splitOneOf(",; \"") cadena)
