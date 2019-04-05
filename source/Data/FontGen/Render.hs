--


module Data.FontGen.Render
  ( renderGlyphs
  , renderString
  , renderStrings
  )
where

import Data.Char
import Data.FontGen.FontType
import Data.FontGen.GlyphType
import Data.FontGen.Util
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Diagrams.Backend.SVG
import Diagrams.Prelude
import System.FilePath


styleGlyph :: Glyph -> Glyph
styleGlyph = lineWidth none . fillColor black

outputDir :: FontInfo -> FilePath
outputDir info = outputRoot </> dir
  where
    outputRoot = "out"
    dir = map toLower (family info) ++ "-" ++ map toLower (weight info)

renderGlyph :: FontInfo -> Char -> Glyph -> IO ()
renderGlyph info char glyph = renderPretty path absolute $ styleGlyph glyph
  where
    path = outputDir info </> show (ord char) <.> "svg"

renderGlyphs :: FontInfo -> IO ()
renderGlyphs info = Map.traverseWithKey (renderGlyph info) (glyphs info) >> return ()

renderString :: FontInfo -> String -> IO ()
renderString info string = renderPretty path absolute diagram
  where
    path = outputDir info </> "test" <.> "svg"
    diagram = scale 0.15 $ hcat $ mapMaybe make string
    make char = styleGlyph <$> Map.lookup char (glyphs info)

renderStrings :: FontInfo -> [String] -> IO ()
renderStrings info strings = renderPretty path absolute diagram
  where
    path = outputDir info </> "test" <.> "svg"
    diagram = scale 0.15 $ vsep 200 $ map (hcat . mapMaybe make) strings
    make char = styleGlyph <$> Map.lookup char (glyphs info)