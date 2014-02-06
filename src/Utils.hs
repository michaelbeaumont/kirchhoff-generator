{-# LANGUAGE OverloadedStrings #-}

module Utils where
import Text.Printf

import Data.Text (Text)
import qualified Data.Text as T

-- Remove nth item from list
remove :: Int -> [a] -> [a]
remove n xs = let (ys,zs) = splitAt n xs in ys ++ (tail zs)

showT :: (Show a) => a -> Text
showT = T.pack . show

numberToEuropean :: Text -> Text
numberToEuropean = T.map switchDelimiters
    where switchDelimiters ',' = '.'
          switchDelimiters '.' = ','
          switchDelimiters x = x

rawFormat :: Double -> Text
rawFormat = T.pack . printf "%.2f"

-- output utils
eurFormatVal :: Double -> Text
eurFormatVal = numberToEuropean . rawFormat
