module Main (
  main
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import qualified Enigma.Character as C
import qualified Enigma.Machine as M
import qualified Enigma.Plugboard as P
import qualified Enigma.Reflector as Re
import qualified Enigma.Rotor as Ro
import System.IO

main :: IO ()
main = do state <- readMachine
          evalStateT interactiveCrypt state


readPlugboard :: [(C.EnigmaChar, C.EnigmaChar)] -> IO [(C.EnigmaChar, C.EnigmaChar)]
readPlugboard pairs = do hFlush stdout
                         line <- getLine
                         if (length line == 0) then return . reverse $ pairs
                                               else do parsedLine <- runExceptT (parsePlugboardLine [] line)
                                                       either failure success parsedLine
  where failure message = do putStrLn message
                             readPlugboard pairs
        success pairs' =  readPlugboard $ pairs' ++ pairs

parsePlugboardLine :: (Monad m) => [(C.EnigmaChar, C.EnigmaChar)] -> [Char] -> ExceptT [Char] m [(C.EnigmaChar, C.EnigmaChar)]
parsePlugboardLine pairs [] = return pairs
parsePlugboardLine pairs (' ':line) = parsePlugboardLine pairs line
parsePlugboardLine pairs (a:' ':_) = throwE $ "Incomplete plugboard pair: " ++ [a]
parsePlugboardLine pairs (a:b:line) = do pair <- characterError $ do a' <- C.enigmaChar a
                                                                     b' <- C.enigmaChar b
                                                                     return (a', b')
                                         parsePlugboardLine (pair:pairs) line
parsePlugboardLine pairs (a:[]) = throwE $ "Incomplete plugboard pair: " ++ [a]

characterError :: (Monad m) => ExceptT C.InvalidCharacterException m a -> ExceptT [Char] m a
characterError c = catchE c translateError
  where translateError e = throwE $ "Invalid character: " ++ (maybe "Unknown" (:[]) $ C.invalidCharacter e)

buildPlugboardFromPairs :: (Monad m) => [(C.EnigmaChar, C.EnigmaChar)] -> ExceptT [Char] m (P.Plugboard C.EnigmaChar)
buildPlugboardFromPairs = plugboardError . P.makePlugboard

plugboardError :: (Monad m) => ExceptT (P.PlugboardError C.EnigmaChar) m b -> ExceptT [Char] m b
plugboardError c = catchE c translateError
  where translateError e = throwE $ "Duplicate character: " ++ (maybe "Unknown" (\e' -> (C.unEnigmaChar e'):[]) $ P.duplicateCharacterError e)

inputPlugboard :: IO (P.Plugboard C.EnigmaChar)
inputPlugboard = do putStrLn "Enter plugboard configuration (pairs of letters optionally separated by spaces or newlines, blank line ends)"
                    plugboardPairs <- readPlugboard []
                    pb <- runExceptT (buildPlugboardFromPairs plugboardPairs)
                    either failure return pb
  where failure message = do putStrLn message
                             inputPlugboard

showPlugboard :: P.Plugboard C.EnigmaChar -> [Char]
showPlugboard = showMappings . P.plugboardMappings
  where showMappings [] = []
        showMappings ((a, b):mappings) = (C.unEnigmaChar a):(C.unEnigmaChar b):' ':(showMappings mappings)

inputReflector :: IO ([Char], Re.Reflector C.EnigmaChar)
inputReflector = do putStr "Please choose a reflector (A, B, C): "
                    readReflector

readReflector :: IO ([Char], Re.Reflector C.EnigmaChar)
readReflector = do hFlush stdout
                   line <- getLine
                   getReflector line
  where getReflector r
          | r == "A" = return $ ("A", C.enigmaIreflectorA)
          | r == "B" = return $ ("B", C.enigmaIreflectorB)
          | r == "C" = return $ ("C", C.enigmaIreflectorC)
          | otherwise = do putStrLn $ "Unknown reflector: " ++ r
                           inputReflector

inputRotorPlacement :: Int -> IO ([Char], (C.EnigmaChar, Ro.Rotor C.EnigmaChar))
inputRotorPlacement i = do (r, rf) <- inputRotor i'
                           rs <- inputRingSetting i'
                           p <- inputPosition i'
                           return $ (r, (p, rf rs))
  where i' = show i

inputRotor :: [Char] -> IO ([Char], C.EnigmaChar -> Ro.Rotor C.EnigmaChar)
inputRotor label = do putStr $ "Please choose a rotor for position " ++ label ++ " (I, II, III, IV, V): "
                      readRotor label

readRotor :: [Char] -> IO ([Char], C.EnigmaChar -> Ro.Rotor C.EnigmaChar)
readRotor label = do hFlush stdout
                     line <- getLine
                     getRotor line
  where getRotor r
          | r == "I" = return ("I", C.enigmaIrotorI)
          | r == "II" = return ("II", C.enigmaIrotorII)
          | r == "III" = return ("III", C.enigmaIrotorIII)
          | r == "IV" = return ("IV", C.enigmaIrotorIV)
          | r == "V" = return ("V", C.enigmaIrotorV)
          | otherwise = do putStrLn $ "Unknown rotor: " ++ r
                           inputRotor label

inputRingSetting :: [Char] -> IO C.EnigmaChar
inputRingSetting i = inputSingleCharacterSetting ("Rotor " ++ i ++ " ring setting: ") "Ring setting should be one character"

inputPosition :: [Char] -> IO C.EnigmaChar
inputPosition i = inputSingleCharacterSetting ("Rotor " ++ i ++ " position: ") "Position should be one character"

inputSingleCharacterSetting :: [Char] -> [Char] -> IO C.EnigmaChar
inputSingleCharacterSetting prompt errorMsg = do putStr prompt
                                                 hFlush stdout
                                                 line <- getLine
                                                 if (length line == 1) then do let char = head line
                                                                               runExceptT (characterError $ C.enigmaChar char) >>= either failure return
                                                                       else do putStrLn errorMsg
                                                                               inputSingleCharacterSetting prompt errorMsg
  where failure message = do putStrLn message
                             inputSingleCharacterSetting prompt errorMsg

readMachine :: IO (M.Machine C.EnigmaChar, [Char], [[Char]])
readMachine = do pb <- inputPlugboard
                 (reflectorLabel, r) <- inputReflector
                 rotorList <- sequence $ fmap inputRotorPlacement [1, 2, 3]
                 let (rotorLabels, rp) = unzip rotorList
                 let machine = M.makeMachine rp r pb
                 return (machine, reflectorLabel, rotorLabels)

showMachine :: M.Machine C.EnigmaChar -> [Char] -> [[Char]] -> IO ()
showMachine m r rn = do let pb = M.plugboard m
                        putStrLn $ "Plugboard: " ++ showPlugboard pb
                        putStrLn $ "Reflector: " ++ r
                        sequence $ fmap (\(l, p, r) -> do putStr "Rotor "
                                                          putStr l
                                                          putStr ", Ring Setting "
                                                          putStr r
                                                          putStr ", Position "
                                                          putStrLn p)
                                        (zip' (M.placements m) rn)
                        return ()
  where zip' [] [] = []
        zip' (rp:rps) (label:labels) = (label, [C.unEnigmaChar . M.position $ rp], [C.unEnigmaChar . Ro.ringSetting . M.rotor $ rp]):(zip' rps labels)

showMachineT :: M.Machine C.EnigmaChar -> [Char] -> [[Char]] -> StateT s IO ()
showMachineT m re ro = lift $ showMachine m re ro

interactiveCrypt :: StateT (M.Machine C.EnigmaChar, [Char], [[Char]]) IO ()
interactiveCrypt = do putStrLnT "Enter text to encrypt/decrypt and push enter"
                      putStrLnT ":show to show show current state, :set to enter new state, blank line to quit"
                      readAndProcessLine

readAndProcessLine :: StateT (M.Machine C.EnigmaChar, [Char], [[Char]]) IO ()
readAndProcessLine = do putStrT "> "
                        hFlushT stdout
                        line <- getLineT
                        handleLine line
  where handleLine line
          | line == "" = return ()
          | line == ":help" = interactiveCrypt
          | line == ":quit" = return ()
          | line == ":set" = do machine <- readMachineT
                                put machine
                                readAndProcessLine
          | line == ":show" = do (machine, reflector, rotors) <- get
                                 showMachineT machine reflector rotors
                                 readAndProcessLine
          | otherwise = do eitherParsedLine <- runExceptT (characterError . C.enigmaString . filter (/= ' ') $ line)
                           let success chars = do (m, re, ro) <- get
                                                  (cryptChars, m') <- runStateT (M.cryptList chars) m
                                                  put (m', re, ro)
                                                  return . C.unEnigmaString $ cryptChars
                           message <- either return success eitherParsedLine
                           putStrLnT message
                           readAndProcessLine

getLineT :: StateT s IO [Char]
getLineT = lift getLine

hFlushT :: Handle -> StateT s IO ()
hFlushT = lift . hFlush

putStrT :: [Char] -> StateT s IO ()
putStrT = lift . putStr

putStrLnT :: [Char] -> StateT s IO ()
putStrLnT = lift . putStrLn

readMachineT :: StateT (M.Machine C.EnigmaChar, [Char], [[Char]]) IO (M.Machine C.EnigmaChar, [Char], [[Char]])
readMachineT = lift readMachine
