module Main where

import Hate
import Hate.Graphics
import Hate.Fonts.Loader
import Hate.Fonts
import Debug.Trace

data SampleState = SampleState {
    fontSprite :: Sprite,
    fontData :: FontData
}

sampleLoad :: LoadFn SampleState
sampleLoad = SampleState <$> loadSprite "Arial_0.png"
                         <*> loadFontData "Arial.fnt"

sampleDraw :: DrawFn SampleState
sampleDraw s = hatePrint (fontData s, fontSprite s) "Haters gonna Hate" 

sampleUpdate :: UpdateFn SampleState
sampleUpdate _ = return ()

config :: Config
config = 
    Config
        { windowTitle = "Sample - Fonts"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw

