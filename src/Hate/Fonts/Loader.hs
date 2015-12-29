module Hate.Fonts.Loader where

import Hate.Math
import Hate.Fonts.Types
import Text.XML.Light
import Data.Maybe
import qualified Data.Map as Map
import Data.Char (chr)
import Debug.Trace

type Path = String

data BMFont = BMFont BMHeader [BMCharData] deriving (Show, Eq)
data BMHeader = BMHeader {
    fontFace :: String,
    charset :: String,
    scaleW :: Int,
    scaleH :: Int
    } deriving (Show, Eq)

data BMCharData = BMCharData {
    charId :: Int,
    x :: Int,
    y :: Int,
    width :: Int,
    height :: Int,
    xoffset :: Int,
    yoffset :: Int,
    xadv :: Int
    } deriving (Show, Eq)

toFontData :: BMFont -> FontData
toFontData (BMFont header cs) = Map.fromList . map toCharData $ cs
    where
        toCharData (BMCharData c x y w h ox oy xa) = (chr c, CharData { 
            region = CharacterRegion (Vec2 fx fy) (Vec2 fw fh),
            offset = Vec2 (fi ox) (fi oy),
            xadvance = fi xa
            })
          where
            fi = fromIntegral
            fx = fi x / fi (scaleW header)
            fy = fi y / fi (scaleH header)
            fw = fi (x+w) / fi (scaleW header)
            fh = fi (y+h) / fi (scaleH header)


xcharToChar :: Element -> Maybe BMCharData
xcharToChar elem = BMCharData
    <$> attrI "id"
    <*> attrI "x"
    <*> attrI "y"
    <*> attrI "width"
    <*> attrI "height"
    <*> attrI "xoffset"
    <*> attrI "yoffset"
    <*> attrI "xadvance"
    where
        attrI :: String -> Maybe Int
        attrI n = read <$> (findAttr $ unqual n) elem

header :: Element -> BMHeader
header root = BMHeader {
        fontFace = fromJust . findAttr (unqual "face") $ xinfo,
        charset = fromJust . findAttr (unqual "charset") $ xinfo,
        scaleW = read . fromJust . findAttr (unqual "scaleW") $ xcommon,
        scaleH = read . fromJust . findAttr (unqual "scaleH") $ xcommon
    }
    where
        xinfo = fromJust $ findElement (unqual "info") root
        xcommon = fromJust $ findElement (unqual "common") root

font :: Element -> BMFont
font root = fontData
    where
        -- read the char data
        xchars = findElements (unqual "char") root
        charData = map (fromJust . xcharToChar) xchars

        fontData = BMFont (header root) (charData)

loadBMFont :: Path -> IO BMFont
loadBMFont p = do
    fileData <- readFile p
    let contents = parseXML fileData
    let root = (head . tail $ onlyElems contents)
    return $ font root

loadFontData :: Path -> IO FontData
loadFontData p = toFontData <$> loadBMFont p




