module Hate.Fonts where

import qualified Hate.Graphics as Hate
import qualified Data.Map as Map
import Data.Maybe

data CharacterRegion = CharacterRegion (Int, Int) (Int, Int)

data Font = Font (Map.Map Char CharacterRegion)

-- | A pair of x-offset, region to sample
type HateText = [(Int, CharacterRegion)]

toRegion :: Font -> Char -> Maybe CharacterRegion
toRegion f c = lookup c f

toText :: String -> HateText
toText str = zip offsets regions
    where
        regions = catMaybes . map toRegion $ str

        offsets = scanl calcOffset 0 regions

        -- we calculate width of each character and add it to the accumulator
        calcOffset :: Int -> CharacterRegion -> Int
        calcOffset off (CharacterRegion (x1, _) (x2, _)) = off + (x2 - x1)

renderText :: Hate.Sprite -> HateText -> [Hate.DrawRequest]
renderText s txt = map toDR txt
    where
        toDR :: (Int, CharacterRegion) -> Hate.DrawRequest
        toDR (off, (CharacterRegion (x1,y1) (x2,y2))) = 
            Hate.translate (Vec2 (fromIntegral off, 0)) $
            Hate.spritePart s ((x1, y1), (x2,y2))

print :: Hate.Sprite -> String -> [Hate.DrawRequest]
print spr = renderText spr . toText
