--


module Ziphil.Font.Core
  ( renderAllGlyphs
  , renderGlyph
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Diagrams.Backend.SVG
import Diagrams.Prelude as Diagrams
import System.FilePath
import Ziphil.Font.Util


renderAllGlyphs :: FilePath -> Map Int Glyph -> IO ()
renderAllGlyphs path correspondence = Map.traverseWithKey renderGlyph' correspondence >> return ()
  where
    renderGlyph' code glyph = renderGlyph (path </> show code <.> "svg") glyph

renderGlyph :: FilePath -> Glyph -> IO ()
renderGlyph path glyph = renderPretty path absolute $ (lineWidth none . fillColor black) glyph