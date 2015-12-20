module Main where

import Hate.Fonts.Loader

main = do
    f <- loadFont "Arial.fnt"
    print f

