--


module Data.FontGen.Render
  ( renderGlyph
  , renderAllGlyphs
  , renderString
  , renderStrings
  )
where

import Data.Char
import Data.FontGen.Type
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

renderAllGlyphs :: FilePath -> Map Char Glyph -> IO ()
renderAllGlyphs path correspondence = Map.traverseWithKey renderGlyph' correspondence >> return ()
  where
    renderGlyph' char glyph = renderGlyph (path </> show (ord char) <.> "svg") glyph

renderString :: FilePath -> String -> Map Char Glyph -> IO ()
renderString path string correspondence = renderPretty path absolute diagram
  where
    diagram = scale 0.15 $ hcat $ mapMaybe make string
    make char = styleGlyph <$> Map.lookup char correspondence

renderStrings :: FilePath -> [String] -> Map Char Glyph -> IO ()
renderStrings path strings correspondence = renderPretty path absolute diagram
  where
    diagram = scale 0.15 $ vsep 200 $ map (hcat . mapMaybe make) strings
    make char = styleGlyph <$> Map.lookup char correspondence