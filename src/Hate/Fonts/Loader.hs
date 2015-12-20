module Hate.Fonts.Loader where

import Hate.Fonts.Types
import Text.XML.Light
import Data.Maybe

type Path = String

data BMFont = BMFont BMHeader [BMCharData] deriving (Show, Eq)
data BMHeader = BMHeader {
    fontFace :: String,
    charset :: String,
    scaleW :: Int,
    scaleH :: Int
    } deriving (Show, Eq)

data BMCharData = BMCharData {
    id :: Int,
    x :: Int,
    y :: Int,
    width :: Int,
    height :: Int,
    xoffset :: Int,
    yoffset :: Int,
    xadvance :: Int
    } deriving (Show, Eq)


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

loadFont :: Path -> IO BMFont
loadFont p = do
    fileData <- readFile p
    let contents = parseXML fileData
    let root = (head . tail $ onlyElems contents)
    return $ font root




