{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}


module Data.FontGen.Render
  ( createOutputDir
  , renderGlyphs
  , RenderOption, fileName, lineGap, scaleRate
  , renderString
  , renderStrings
  , writeCode
  , generateFont
  )
where

import Control.Monad
import Data.Char
import Data.FontGen.FontType
import Data.FontGen.GlyphType
import Data.FontGen.Util
import Data.FileEmbed
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Encoding
import Data.Version
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (Path, (&~))
import Path
import Path.IO
import System.Exit
import System.Process


styleGlyph :: Glyph -> Diagram B
styleGlyph = lineWidth (global 0) . fillColor black

renderDiagram :: Path Rel File -> Diagram B -> IO ()
renderDiagram path diagram = renderPretty (toFilePath path) absolute diagram

outputDir :: FontInfo -> IO (Path Rel Dir)
outputDir info = (root </>) <$> dir
  where
    root = $(mkRelDir "out")
    dir = parseRelDir $ info ^. dirName

outputFile :: FontInfo -> String -> String -> IO (Path Rel File)
outputFile info name ext = (</>) <$> outputDir info <*> file
  where
    file = (-<.> ext) =<< parseRelFile name

createOutputDir :: FontInfo -> IO ()
createOutputDir info = createDirIfMissing True =<< outputDir info

renderGlyph :: FontInfo -> Char -> Glyph -> IO ()
renderGlyph info char glyph = flip renderDiagram diagram =<< path
  where
    path = outputFile info name "svg"
    name = show (ord char)
    diagram = styleGlyph glyph

renderGlyphs :: FontInfo -> IO ()
renderGlyphs info = Map.traverseWithKey (renderGlyph info) (info ^. glyphs) >> return ()

data RenderOption = RenderOption {_fileName :: String, _lineGap :: Double, _scaleRate :: Double}
  deriving (Eq, Show)

makeFieldsNoPrefix ''RenderOption

instance Default RenderOption where
  def = RenderOption "test" 200 0.1

makeCharDiagram :: FontInfo -> Char -> Diagram B
makeCharDiagram info char = styleGlyph $ fromMaybe mempty $ Map.lookup char (info ^. glyphs)

makeStringDiagram :: RenderOption -> FontInfo -> String -> Diagram B
makeStringDiagram option info string = hcat $ map (makeCharDiagram info) string

renderString :: RenderOption -> FontInfo -> String -> IO ()
renderString option info string = flip renderDiagram diagram =<< path
  where
    path = outputFile info (option ^. fileName) "svg"
    diagram = scale (option ^. scaleRate) $ makeStringDiagram option info string

makeStringsDiagram :: RenderOption -> FontInfo -> [String] -> Diagram B
makeStringsDiagram option info strings = vsep (option ^. lineGap) $ map (makeStringDiagram option info) strings

renderStrings :: RenderOption -> FontInfo -> [String] -> IO ()
renderStrings option info strings = flip renderDiagram diagram =<< path
  where
    path = outputFile info (option ^. fileName) "svg"
    diagram = scale (option ^. scaleRate) $ makeStringsDiagram option info strings

-- テキスト中の与えられた文字列に一致する箇所を変換するためのセッターです。
sub :: Show a => String -> Setter Text Text String a
sub needle = sets sub'
  where
    sub' func = Text.replace (Text.pack needle) (Text.pack $ show $ func needle)

-- テンプレートコード中のメタ変数を変換して、フォント生成用の Python コードを生成します。
makeCode :: FontInfo -> Text
makeCode info = decodeUtf8 $(embedFile "resource/generate.py") &~ do
  sub "__familyname__" .= info ^. family
  sub "__fontname__" .= info ^. fullName
  sub "__fullname__" .= info ^. fullName
  sub "__weight__" .= info ^. style . weight # showWeight
  sub "__version__" .= info ^. version # showVersion
  sub "__em__" .= info ^. metrics . metricEm # truncate
  sub "__ascent__" .= info ^. metrics . metricAscent # truncate
  sub "__descent__" .= info ^. metrics . metricDescent # truncate

writeCode :: FontInfo -> IO ()
writeCode info = flip Text.writeFile code =<< path
  where
    path = toFilePath <$> outputFile info "generate" "py"
    code = makeCode info

generateFont :: FontInfo -> IO ExitCode
generateFont info = system =<< (++ (" & " ++ pythonCommand)) <$> cdCommand
  where
    pythonCommand = "ffpython generate.py"
    cdCommand = ("cd " ++) . toFilePath <$> outputDir info