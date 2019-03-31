--


module Ziphil.Font.Core
  ( renderAllGlyphs
  , renderGlyph
  )
where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Diagrams.Backend.SVG
import Diagrams.Prelude as Diagrams
import System.FilePath
import Ziphil.Font.Util


renderAllGlyphs :: FilePath -> Map Char Glyph -> IO ()
renderAllGlyphs path correspondence = Map.traverseWithKey renderGlyph' correspondence >> return ()
  where
    renderGlyph' char glyph = renderGlyph (path </> show (ord char) <.> "svg") glyph

renderGlyph :: FilePath -> Glyph -> IO ()
renderGlyph path glyph = renderPretty path absolute $ (lineWidth none . fillColor black) glyph