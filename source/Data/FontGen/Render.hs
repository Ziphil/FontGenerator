{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}


module Data.FontGen.Render
  ( createOutputDir
  , renderGlyphs
  , RenderOption, strings, fileName, lineGap, scaleRate
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

-- FontForge に読み込ませるために、ダイアグラムのバウンディングボックスを em 正方形に一致させます。
adjustWidth :: Font -> Diagram B -> Diagram B
adjustWidth font = rectEnvelope base size
  where
    base = (0 &| 0 - font ^. metrics . metricDescent)
    size = (font ^. metrics . metricEm &| font ^. metrics . metricEm)

renderDiagram :: Path Rel File -> Diagram B -> IO ()
renderDiagram path = renderPretty (toFilePath path) absolute

-- Python コードがグリフの幅を取得できるように、指定した値を属性として保持する SVG を生成します。
renderDiagram' :: Path Rel File -> Double -> Diagram B -> IO ()
renderDiagram' path glyphWidth = renderPretty' (toFilePath path) option
  where
    option = SVGOptions absolute Nothing "" [extraAttribute] True
    extraAttribute = makeAttribute "glyph-width" (Text.pack $ show glyphWidth)

outputDir :: Font -> IO (Path Rel Dir)
outputDir font = (root </>) <$> dir
  where
    root = $(mkRelDir "out")
    dir = parseRelDir $ font ^. dirName

outputFile :: Font -> String -> String -> IO (Path Rel File)
outputFile font name ext = (</>) <$> outputDir font <*> file
  where
    file = (-<.> ext) =<< parseRelFile name

createOutputDir :: Font -> IO ()
createOutputDir font = createDirIfMissing True =<< outputDir font

renderGlyph :: Font -> Char -> Glyph -> IO ()
renderGlyph font char glyph = (\path -> renderDiagram' path (width glyph) diagram) =<< path
  where
    path = outputFile font name "svg"
    name = show $ ord char
    diagram = adjustWidth font $ styleGlyph glyph

renderGlyphs :: Font -> IO ()
renderGlyphs font = void $ Map.traverseWithKey (renderGlyph font) (font ^. glyphs)

data RenderOption = RenderOption {_strings :: [String], _fileName :: String, _lineGap :: Double, _scaleRate :: Double}
  deriving (Eq, Show)

makeFieldsNoPrefix ''RenderOption

instance Default RenderOption where
  def = RenderOption [] "test" 200 0.1

makeCharDiagram :: Font -> Char -> Diagram B
makeCharDiagram font = styleGlyph . fromMaybe mempty . flip Map.lookup (font ^. glyphs)

makeStringDiagram :: Font -> String -> Diagram B
makeStringDiagram font = hcat . map (makeCharDiagram font)

renderStrings :: RenderOption -> Font -> IO ()
renderStrings option font = flip renderDiagram diagram =<< path
  where
    path = outputFile font (option ^. fileName) "svg"
    diagram = scale (option ^. scaleRate) rawDiagram
    rawDiagram = vsep (option ^. lineGap) $ map (makeStringDiagram font) $ option ^. strings

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
makeCode :: GenerateOption -> Font -> Text
makeCode option font = decodeUtf8 $(embedFile "resource/generate.py") &~ do
  sub "__familyname__" .= font ^. extendedFamily
  sub "__fontname__" .= font ^. postScriptName
  sub "__fullname__" .= font ^. fullName
  sub "__weight__" .= font ^. style . weight # showWeight
  sub "__version__" .= font ^. version # showVersion
  sub "__copyright__" .= font ^. copyright
  sub "__em__" .= font ^. metrics . metricEm
  sub "__ascent__" .= font ^. metrics . metricAscent
  sub "__descent__" .= font ^. metrics . metricDescent
  sub "__fontfilename__" .= "../" <> font ^. dirName <> ".ttf"

-- フォント生成用 Python コードを生成し、ファイルに書き出します。
writeCode :: GenerateOption -> Font -> IO ()
writeCode option font = flip Text.writeFile code =<< path
  where
    path = toFilePath <$> outputFile font (option ^. codeFileName) "py"
    code = makeCode option font

-- フォント生成用 Python コードを実行して、フォントを生成します。
-- あらかじめ生成用のコードを用意しておいてください。
generateFont :: GenerateOption -> Font -> IO ()
generateFont option font = void . system =<< (<> (" & " <> pythonCommand)) <$> cdCommand
  where
    pythonCommand = option ^. command <> " " <> path
    cdCommand = ("cd " <>) . toFilePath <$> outputDir font
    path = option ^. codeFileName <> ".py"

-- グリフ生成からフォント生成までの一連の処理を全て行います。
generateAll :: GenerateOption -> Font -> IO ()
generateAll option font = do
  createOutputDir font
  renderGlyphs font
  writeCode option font
  generateFont option font