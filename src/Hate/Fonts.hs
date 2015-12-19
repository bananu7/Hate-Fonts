module Hate.Fonts where

import qualified Hate.Graphics as Hate
import Hate.Math (Vec2(..))
import qualified Data.Map as Map
import Data.Maybe

data CharacterRegion = CharacterRegion Vec2 Vec2

type Font = Map.Map Char CharacterRegion

-- | A pair of x-offset, region to sample
type HateText = [(Float, CharacterRegion)]

toRegion :: Font -> Char -> Maybe CharacterRegion
toRegion f c = Map.lookup c f

toText :: Font -> String -> HateText
toText f str = zip offsets regions
    where
        regions = catMaybes . map (toRegion f) $ str

        offsets = scanl calcOffset 0 regions

        -- we calculate width of each character and add it to the accumulator
        calcOffset :: Float -> CharacterRegion -> Float
        calcOffset off (CharacterRegion (Vec2 x1 _) (Vec2 x2 _)) = off + (x2 - x1)

renderText :: Hate.Sprite -> HateText -> [Hate.DrawRequest]
renderText s txt = map toDR txt
    where
        toDR :: (Float, CharacterRegion) -> Hate.DrawRequest
        toDR (off, (CharacterRegion a b)) = 
            Hate.translate (Vec2 off 0) $ Hate.spritePart (a, b) s

print :: Font -> Hate.Sprite -> String -> [Hate.DrawRequest]
print f spr = renderText spr . toText f
