{-# LANGUAGE TemplateHaskell #-}


module Data.FontGen.Render
  ( createOutputDir
  , renderGlyphs
  , RenderOption (..)
  , renderString
  , renderStrings
  )
where

import Control.Monad
import Data.Char
import Data.FontGen.FontType
import Data.FontGen.GlyphType
import Data.FontGen.Util
import qualified Data.Map as Map
import Data.Maybe
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (Path)
import Path
import Path.IO


styleGlyph :: Glyph -> Diagram B
styleGlyph = lineWidth none . fillColor black

renderDiagram :: FilePath -> Diagram B -> IO ()
renderDiagram path diagram = renderPretty path absolute diagram

outputDir :: FontInfo -> IO (Path Rel Dir)
outputDir info = (root </>) <$> dir
  where
    root = $(mkRelDir "out")
    dir = parseRelDir $ dirName info

outputFile :: FontInfo -> String -> IO (Path Rel File)
outputFile info name = (</>) <$> outputDir info <*> file
  where
    file = (-<.> "svg") =<< parseRelFile name

createOutputDir :: FontInfo -> IO ()
createOutputDir info = createDirIfMissing True =<< outputDir info

renderGlyph :: FontInfo -> Char -> Glyph -> IO ()
renderGlyph info char glyph = join $ renderDiagram <$> path <*> return (styleGlyph glyph)
  where
    path = toFilePath <$> outputFile info name
    name = show (ord char)

renderGlyphs :: FontInfo -> IO ()
renderGlyphs info = Map.traverseWithKey (renderGlyph info) (glyphs info) >> return ()

data RenderOption = RenderOption {fileName :: String, lineGap :: Double, scaleRate :: Double}
  deriving (Eq, Show)

renderString :: RenderOption -> FontInfo -> String -> IO ()
renderString option info string = join $ renderDiagram <$> path <*> return diagram
  where
    path = toFilePath <$> outputFile info (fileName option)
    diagram = scale (scaleRate option) $ hcat $ mapMaybe make string
    make char = styleGlyph <$> Map.lookup char (glyphs info)

renderStrings :: RenderOption -> FontInfo -> [String] -> IO ()
renderStrings option info strings = join $ renderDiagram <$> path <*> return diagram
  where
    path = toFilePath <$> outputFile info (fileName option)
    diagram = scale (scaleRate option) $ vsep (lineGap option) $ map (hcat . mapMaybe make) strings
    make char = styleGlyph <$> Map.lookup char (glyphs info)