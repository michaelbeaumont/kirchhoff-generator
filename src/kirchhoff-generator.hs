{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import System.Random
import System.Cmd
import System.Environment
import Data.Functor
import Data.List
import Data.Monoid
import Control.Monad
import Control.Exception

import Types
import Latex
import Test
import Utils (showT)

import qualified Data.Text.IO as T

import Testing
import Data.Maybe (fromJust)

--Set to True for Debug info
debug' :: Bool
debug' = False

commandlineOptions :: String
commandlineOptions = concat ["\nOptionen:\ngenerate [Nummer für die erste Frage] [Anzahl von Fragen] "
         ,"[Anzahl von Resistoren] [DEBUG]\nSetzen Sie GENERATOR_DEBUG auf True "
         ,"oder fügen Sie noch ein 4. Argument hinzu, "
         ,"um zusätzliche Debug-Ausgabe auszugeben"]


main = do
    debugEnv <- (getEnv "GENERATOR_DEBUG") `catch`
                    (\err -> const (return "False") (err :: SomeException))
    args <- getArgs
    let debugArg = length args > 3
    when (length args < 3)
        (putStrLn commandlineOptions)
    let debug = debug' || read debugEnv || debugArg
        (startQuestion, numQuestions, numResistors) =
         if length args < 2 then (1, 1, 9)
                            else (read $ args !! 0, read $ args !! 1, read $ args !! 2)
                    --this is an inefficient way to read args, ie O(n^2)
    repeatAction numQuestions (main_sub debug startQuestion numResistors)
    putStrLn commandlineOptions
    where repeatAction 0 _ = return ()
          repeatAction n action = action n >> repeatAction (n-1) action


main_sub debug startQuestion numResistors n = do
    let questionNum = startQuestion + n - 1
    sg <- getStdGen
    let getVSource = getStdRandom $ randomR (10,100) :: IO Int
    let getCSource = getStdRandom $ randomR (1,3) :: IO Int
    let getRes = getStdRandom $ randomR (50,200) :: IO Int
    res <- fmap fromIntegral getRes
    vSource <- fromIntegral <$> getVSource
    cSource <- fromIntegral <$> getCSource
    isVoltage <- getStdRandom random
    isParallel <- getStdRandom random
    --let isVoltage = False
    --isParallel = True
    let source =
            if isVoltage
                 then VoltageSource vSource
                 else CurrentSource cSource

        start' = splitBlock (compTree isParallel Forward numResistors (Value res) 0 0 []) sg
        (start, _) = case calculate (start', Source source) of--power source start'
                        Nothing -> (start', Source source)
                        (Just r) -> r
        outputFunc = if debug then blockToLatex'
                              else blockToLatex
        buildDir = "generated"
        filename = buildDir++"/question"++show questionNum
        intersperseFilename = concat . intersperse filename
        pdflatex = intersperseFilename ["pdflatex -output-directory "++buildDir++" ",".tex"]
        convert = intersperseFilename
                    ["convert -density 300 ",
                     ".pdf -quality 100 ",
                     ".png"]
        remove = intersperseFilename ["rm ",".pdf ",".log ",".aux ",".tex"]
    writeLatexToFile outputFunc (filename++".tex") (RoseNode $ Source source) start
    void $ system $ concat . intersperse "&& " $ [pdflatex,convert,remove]
    when debug (do
                    T.putStrLn $ "\nQuestion " <> showT questionNum
                    print start
                    --putStr $ unlines . reverse $ buildGiven questions
                    T.putStrLn "----"
                    --putStr $ unlines . reverse $ buildQuestions questions
                )


testSrc = VoltageSource 5
testBlock = (fst . fromJust ) $ calculate hugeSrcNetz
--testBlock = (snd) $ powerNetz hugeSrcNetz
--testBlock = (snd) $ fromSrc--calculate hugeSrcNetz
testPic = writeLatexToFile blockToLatex' (name ++".tex") (RoseNode $ Source testSrc) testBlock >>
                 system ("pdflatex "++name++".tex") >>
                 system ("convert -density 300 "++name++".pdf -quality 100 "++name++".png")
    where name = "test2"

fromSrc = powerNetz (fromJust $ liftUp (findAllSources hugeSrcNetz !! 0))
