module Hate.Fonts
    ( module Hate.Fonts.Types
    , hatePrint
    )
where

import qualified Hate.Graphics as Hate
import Hate.Math (Vec2(..))
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

import Hate.Fonts.Types

toCharData :: Font -> Char -> Maybe CharData
toCharData f c = Map.lookup c f

-- | A pair of x-offset, region to sample
type HateText = [(Float, CharData)]

toText :: Font -> String -> HateText
toText f str = zip offsets regions
    where
        regions = catMaybes . map (toCharData f) $ str

        offsets = scanl calcOffset 0 regions

        -- we use the xoffset data of each character to advance the offset
        calcOffset :: Float -> CharData -> Float
        calcOffset off (CharData _ _ xoff) = off + xoff

renderText :: Hate.Sprite -> HateText -> [Hate.DrawRequest]
renderText s txt = map toDR txt
    where
        toDR :: (Float, CharData) -> Hate.DrawRequest
        toDR (textOffset, CharData (CharacterRegion a b) (Vec2 ox oy) xadv) = 
            Hate.translate (Vec2 (ox+textOffset) oy) $ (Hate.spritePart (a, b) s)

hatePrint :: Font -> Hate.Sprite -> String -> [Hate.DrawRequest]
hatePrint f spr = renderText spr . toText f
