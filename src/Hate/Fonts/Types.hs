module Hate.Fonts.Types where

import qualified Data.Map as Map
import Hate.Math (Vec2(..))

data CharacterRegion = CharacterRegion Vec2 Vec2

type Font = Map.Map Char CharacterRegion

-- | A pair of x-offset, region to sample
type HateText = [(Float, CharacterRegion)]