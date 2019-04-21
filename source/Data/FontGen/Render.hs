{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}


module Data.FontGen.Render
  ( createOutputDir
  , renderGlyphs
  , RenderOption, fileName, lineGap, scaleRate
  , renderString
  , renderStrings
  , GenerateOption, codeFileName, command
  , writeCode
  , generateFont
  , generateAll
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
import Graphics.Svg.Core
import Path
import Path.IO
import System.Exit
import System.Process


styleGlyph :: Glyph -> Diagram B
styleGlyph = lineWidth (global 0) . fillColor black

adjustWidth :: FontInfo -> Diagram B -> Diagram B
adjustWidth info = rectEnvelope base size
  where
    base = (0 &| 0 - info ^. metrics . metricDescent)
    size = (info ^. metrics . metricEm &| info ^. metrics . metricEm)

renderDiagram :: Path Rel File -> Diagram B -> IO ()
renderDiagram path = renderPretty (toFilePath path) absolute

renderDiagram' :: Path Rel File -> Double -> Diagram B -> IO ()
renderDiagram' path glyphWidth = renderPretty' (toFilePath path) option
  where
    option = SVGOptions absolute Nothing "" [extraAttribute] True
    extraAttribute = makeAttribute "glyph-width" (Text.pack $ show glyphWidth)

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
renderGlyph info char glyph = (\path -> renderDiagram' path (width glyph) diagram) =<< path
  where
    path = outputFile info name "svg"
    name = show $ ord char
    diagram = adjustWidth info $ styleGlyph glyph

renderGlyphs :: FontInfo -> IO ()
renderGlyphs info = void $ Map.traverseWithKey (renderGlyph info) (info ^. glyphs)

data RenderOption = RenderOption {_fileName :: String, _lineGap :: Double, _scaleRate :: Double}
  deriving (Eq, Show)

makeFieldsNoPrefix ''RenderOption

instance Default RenderOption where
  def = RenderOption "test" 200 0.1

makeCharDiagram :: FontInfo -> Char -> Diagram B
makeCharDiagram info char = styleGlyph $ fromMaybe mempty $ Map.lookup char (info ^. glyphs)

makeStringDiagram :: RenderOption -> FontInfo -> String -> Diagram B
makeStringDiagram option info = hcat . map (makeCharDiagram info)

renderString :: RenderOption -> FontInfo -> String -> IO ()
renderString option info string = flip renderDiagram diagram =<< path
  where
    path = outputFile info (option ^. fileName) "svg"
    diagram = scale (option ^. scaleRate) $ makeStringDiagram option info string

makeStringsDiagram :: RenderOption -> FontInfo -> [String] -> Diagram B
makeStringsDiagram option info = vsep (option ^. lineGap) . map (makeStringDiagram option info)

renderStrings :: RenderOption -> FontInfo -> [String] -> IO ()
renderStrings option info strings = flip renderDiagram diagram =<< path
  where
    path = outputFile info (option ^. fileName) "svg"
    diagram = scale (option ^. scaleRate) $ makeStringsDiagram option info strings

data GenerateOption = GenerateOption {_codeFileName :: String, _command :: String}
  deriving (Eq, Show)

makeFieldsNoPrefix ''GenerateOption

instance Default GenerateOption where
  def = GenerateOption "generate" "ffpython"

-- テキスト中の与えられた文字列に一致する箇所を変換するためのセッターです。
sub :: Show a => String -> Setter Text Text String a
sub needle = sets sub'
  where
    sub' func = Text.replace (Text.pack needle) (Text.pack $ show $ func needle)

-- テンプレートコード中のメタ変数を変換して、フォント生成用の Python コードを生成します。
makeCode :: GenerateOption -> FontInfo -> Text
makeCode option info = decodeUtf8 $(embedFile "resource/generate.py") &~ do
  sub "__familyname__" .= info ^. extendedFamily
  sub "__fontname__" .= info ^. fullName
  sub "__fullname__" .= info ^. fullName
  sub "__weight__" .= info ^. style . weight # showWeight
  sub "__version__" .= info ^. version # showVersion
  sub "__copyright__" .= info ^. copyright
  sub "__em__" .= info ^. metrics . metricEm
  sub "__ascent__" .= info ^. metrics . metricAscent
  sub "__descent__" .= info ^. metrics . metricDescent
  sub "__fontfilename__" .= "../" <> info ^. dirName <> ".ttf"

-- フォント生成用 Python コードを生成し、ファイルに書き出します。
writeCode :: GenerateOption -> FontInfo -> IO ()
writeCode option info = flip Text.writeFile code =<< path
  where
    path = toFilePath <$> outputFile info (option ^. codeFileName) "py"
    code = makeCode option info

-- フォント生成用 Python コードを実行して、フォントを生成します。
-- あらかじめ生成用のコードを用意しておいてください。
generateFont :: GenerateOption -> FontInfo -> IO ()
generateFont option info = void . system =<< (<> (" & " <> pythonCommand)) <$> cdCommand
  where
    pythonCommand = option ^. command <> " " <> path
    cdCommand = ("cd " <>) . toFilePath <$> outputDir info
    path = option ^. codeFileName <> ".py"

-- グリフ生成からフォント生成までの一連の処理を全て行います。
generateAll :: GenerateOption -> FontInfo -> IO ()
generateAll option info = do
  createOutputDir info
  renderGlyphs info
  writeCode option info
  generateFont option info