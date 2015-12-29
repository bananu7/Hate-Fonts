module Hate.Fonts.Types where

import qualified Data.Map as Map
import Hate.Math (Vec2(..))
import Hate.Graphics (Sprite)

data CharacterRegion = CharacterRegion Vec2 Vec2 deriving (Show, Eq)

data CharData = CharData {
    region :: CharacterRegion,
    offset :: Vec2,
    xadvance :: Float
} deriving (Show, Eq)

type FontData = Map.Map Char CharData
type Font = (FontData, Sprite)
