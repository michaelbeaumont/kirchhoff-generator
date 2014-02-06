{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Latex where
import Prelude hiding (readFile)
import System.IO (Handle, withFile, IOMode(..))
import Data.Text.IO
import Types
import Utils
import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T

import Paths_kirchhoff_generator

type CompTreeOutputter = CompTree -> Int -> Text

--LATEX GENERATING FUNCTIONS--
blockToLatex' :: CompTreeOutputter
blockToLatex' (RoseNode (Resistor _ res vol cur)) _ = 
    "to[R, l_=$" <> showRes res
    <> "\\ohm$, i>^=$" <> (escapeComma . eurFormatVal $ cur *1000)
    <> "mA$, v^>=$" <> (escapeComma . eurFormatVal $ vol ) <> "V$]"
    where showRes (Value val) = T.pack . show $ val
          showRes r = T.pack . show $ r
blockToLatex' (RoseNode (Source source')) _ = case source' of 
                    VoltageSource v -> "to[european voltage source, v=$"
                        <> showT v <> "V$]"
                    CurrentSource c -> "to[european current source, i=$"
                        <> showT c <> "A$]"
blockToLatex' (TopNode (Source source')) _ = case source' of 
                    VoltageSource v -> "to[european voltage source, v=$"
                        <> showT v <> "V$]"
                    CurrentSource c -> "to[european current source, i=$"
                        <> showT c <> "A$]"
blockToLatex' _ _ = "<CompTree>"


escapeComma :: Text -> Text
escapeComma = T.concatMap escape
    where escape ',' = "{,}"
          escape c = T.singleton c

blockToLatex :: CompTreeOutputter
blockToLatex _ n = "to[R, i>^=$i_{" <> showT n
                <> "}$, v^>=$U_{" <> showT n
                <> "}$]"

-- Blocks grow to the right and down
recToLatex' :: CompTreeOutputter
               -> CompTree
               -> Double
               -> Double
               -> Maybe Double
               -> Int
               -> Text
recToLatex' outputFunc b@(size -> 1) _ _ _ ref =
    outputFunc b ref
    
recToLatex' outputFunc b@(RoseTree (parallel -> False) rs) xoff yoff parEnd ref = 

    blockToLatexWithXOffset rs 0 ref
    <>endDraw

    where recToLatex = recToLatex' outputFunc
          blockToLatexWithXOffset [] x _ = 
            let connector parEnd = 
                  draw <> point (xoff+x) yoff
                  `to` point parEnd yoff
                  <>endDraw 
            in maybe "" connector parEnd

          blockToLatexWithXOffset (b@(size -> 1):bs) x ref = 
            draw <> point (xoff+x) yoff
            `to` point (xoff+x+0.5) yoff
            <>outputFunc b ref
            <>point (xoff+x+2.5) yoff
            `to` point (xoff+x+3) yoff
            <>endDraw
            <>blockToLatexWithXOffset bs (x+2.5) (ref + 1)

          blockToLatexWithXOffset (b:bs) x ref =
            recToLatex b (xoff+x) yoff Nothing ref
            <>blockToLatexWithXOffset bs (x + maxSerial b) (ref + size b)

recToLatex' outputFunc b@(RoseTree (parallel -> True) rs) xoff' yoff parEnd ref =

    draw <> point xoff' yoff
    `to` point xoff yoff
    `to` point xoff (yoff - curMaxPar + 2)
    <>endDraw
    <>blockToLatexWithYOffset rs 0 ref
    <>draw <> point rightHorizontal yoff
    `to` point rightHorizontal (yoff - (curMaxPar - 2))
    <>endDraw

    where curMaxPar = maxVertLineLength b
          xoff = xoff'+0.5 -- start 0.5 further right
          recToLatex = recToLatex' outputFunc
          rightHorizontal = xoff' + maxSerial b
          blockToLatexWithYOffset [] _ _ =
            let connector parEnd =
                  draw <> point rightHorizontal yoff
                  `to` point parEnd yoff
                  <>endDraw 
            in maybe "" connector parEnd

          blockToLatexWithYOffset (b@(size -> 1):bs) y ref =
            draw <> point xoff (yoff-y)
            `to` point (xoff+0.5) (yoff - y)
            <>outputFunc b ref
            <>point (xoff+2.5) (yoff-y)
            `to` point rightHorizontal (yoff-y)
            <>endDraw
            <>blockToLatexWithYOffset bs (y+2) (ref + 1)

          blockToLatexWithYOffset (b:bs) y ref =
            recToLatex b xoff (yoff-y) (Just $ rightHorizontal) ref
            <>blockToLatexWithYOffset bs (y+ maxParallel b) (ref + size b)


--FIND SIZES OF BLOCKS--
maxParallel :: CompTree -> Double
maxParallel (size -> 1) = 2
maxParallel (RoseTree (parallel -> True) rs) =
    foldl (\a r -> maxParallel r + a) 0 rs
maxParallel (RoseTree (parallel -> False) rs) =
    maximum $ map maxParallel rs

maxSerial :: CompTree -> Double
maxSerial (size -> 1) = 2.5
maxSerial (RoseTree (parallel -> True) rs) = 
    1 + (maximum $ map maxSerial rs)
maxSerial (RoseTree (parallel -> False) rs) =
    foldl (\a r -> maxSerial r + a) 0 rs

maxVertLineLength :: CompTree -> Double
maxVertLineLength (size -> 1) = 2
maxVertLineLength (RoseTree (parallel -> True) children) =
    --foldl (\a r -> maxParallel r + a) start rs
    foldIgnoreLast children
    where foldIgnoreLast [] = 0
          foldIgnoreLast [_] = 2
          foldIgnoreLast (r:rs) = maxParallel r + foldIgnoreLast rs
    -- ^^ this is not so great but it works perfectly
maxVertLineLength (RoseTree (parallel -> False) rs) =
    maximum $ map maxVertLineLength rs

--LATEX UTIL FUNCTIONS--
point :: (Show a, Show b) => a -> b -> Text
point x y = " ("<> showT x<>","<> showT y <>") "

pointToPoint :: (Show a, Show b, Show c, Show d) =>
    a -> b -> c -> d -> Text
pointToPoint x y u v = point x y<>" -- "<>point u v

to :: Text -> Text -> Text
p `to` q = p<>" -- "<>q

endDraw :: Text
endDraw = ";\n"

draw :: Text
draw = "\\draw "

--Latex header write to file--
latexWriteHeader :: Handle -> IO ()
latexWriteHeader hdl = do
        lPutStrLn "\\documentclass[border={0pt, 0pt, 10pt, 5pt}]{standalone}"
        lPutStrLn "\\usepackage[free-standing-units]{siunitx}"
        lPutStrLn "\\usepackage[europeanresistors]{circuitikz}"
        lPutStrLn "\\usepackage{tikz}"
        lPutStrLn "\\usepackage{verbatim}"
        lPutStrLn "\\begin{document}"
        lPutStrLn "\\begin{circuitikz}"
        germanVoltage <- getDataFileName "germanvoltage.tex" >>= readFile
        lPutStrLn germanVoltage
        lPutStrLn "\\ctikzset {voltage=german}"
    where lPutStrLn = hPutStrLn hdl

latexWriteFooter :: Handle -> IO ()
latexWriteFooter hdl = do
        lPutStrLn "\\end{circuitikz}"
        lPutStrLn "\\end{document}"
    where lPutStrLn = hPutStrLn hdl

--draw power source for network--
drawTop :: CompTreeOutputter -> Handle -> CompTree -> CompTree -> IO ()
drawTop outputFunc hdl source' block =
        lPutStr $ "" --"\\ctikzset {current/distance = 2};"
                   <> draw <> point 0 0
                    `to` point 0 (-bottom)
                    `to` point (right/2) (-bottom)
                     <> source 
                     <> point (right/2 + 1) (-bottom)
                    `to` point right (-bottom)
                    `to` point right 0
                    `to` point (right-1) 0
                    <> endDraw
                     <> "\\ctikzset {voltage/bump a = 1.5};"
    where lPutStr = hPutStr hdl
          right = maxSerial block + 0.5
          bottom = maxParallel block
          source = outputFunc source' 0 --case source' of 
                    --VoltageSource _ -> "to[european voltage source, v=$U_0$]"
                    --CurrentSource _ -> "to[european current source, i<^=$i_0$]"
                    --hmm have to add debugging for source

writeLatexToFile :: CompTreeOutputter
                    -> FilePath
                    -> CompTree
                    -> CompTree
                    -> IO ()
writeLatexToFile outputFunc path source block = withFile path ReadWriteMode
    (\hdl -> do
        let left = 0
            top = 0
        latexWriteHeader hdl
        drawTop outputFunc hdl source block
        hPutStrLn hdl $ recToLatex block left top (Just $ maxSerial block) 1
        hPutStrLn hdl "\\begin{comment}"
        hPutStrLn hdl $ showT $ block
        hPutStrLn hdl "\\end{comment}"
        latexWriteFooter hdl)
    where recToLatex = recToLatex' outputFunc
