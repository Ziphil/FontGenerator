--


module Data.FontGen.Render
  ( renderGlyph
  , renderAllGlyphs
  , renderString
  , renderStrings
  )
where

import Data.Char
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

renderGlyph :: FilePath -> Glyph -> IO ()
renderGlyph path glyph = renderPretty path absolute $ styleGlyph glyph

renderAllGlyphs :: FilePath -> Glyphs -> IO ()
renderAllGlyphs path glyphs = Map.traverseWithKey renderGlyph' glyphs >> return ()
  where
    renderGlyph' char glyph = renderGlyph (path </> show (ord char) <.> "svg") glyph

renderString :: FilePath -> String -> Glyphs -> IO ()
renderString path string glyphs = renderPretty path absolute diagram
  where
    diagram = scale 0.15 $ hcat $ mapMaybe make string
    make char = styleGlyph <$> Map.lookup char glyphs

renderStrings :: FilePath -> [String] -> Glyphs -> IO ()
renderStrings path strings glyphs = renderPretty path absolute diagram
  where
    diagram = scale 0.15 $ vsep 200 $ map (hcat . mapMaybe make) strings
    make char = styleGlyph <$> Map.lookup char glyphs