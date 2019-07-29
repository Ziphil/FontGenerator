{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Data.FontGen.Glyph
  ( Rim
  , rimBy
  , Part
  , partBy
  , unite
  , Glyph (..)
  , glyphBy
  , Glyphs
  , glyphsBy
  )
where

import Data.Default.Class
import Data.FontGen.Metrics
import Data.FontGen.Util.Core
import Data.FontGen.Util.State
import Data.Map (Map)
import qualified Data.Map as Map
import Diagrams.Backend.SVG
import Diagrams.Prelude


type RimElem = Trail V2 Double
type Rim = MonoidState [RimElem] ()

-- リムからリムを生成します。
-- リムを返す多相関数を do 構文内で使った場合に、型変数の曖昧性を排除するのに利用できます。
rimBy :: Rim -> Rim
rimBy = id

type PartElem = Path V2 Double
type Part = MonoidState [PartElem] ()

-- リムのリストから 1 つのパスから成るパーツを生成します。
-- 生成の際に自動的にパスを閉じるので、リムの始点と終点は同じ点であるようにしてください。
partBy :: Rim -> Part
partBy rims = add [pathFromTrail . closeTrail . mconcat $ execState' rims]

-- 複数のパーツを結合して 1 つのパスから成るパーツにします。
-- 中に空洞がある形をしたパーツを作成するのに利用できます。
unite :: Part -> Part
unite parts = add [mconcat $ execState' parts]

data Glyph = forall m s. ReformEnvelope m s => Glyph Part m s

partIn :: Glyph -> Part 
partIn (Glyph part _ _) = part

-- パーツのリストからグリフを生成します。
glyphBy :: ReformEnvelope m s => m -> s -> Part -> Glyph
glyphBy metrics spacing part = Glyph part metrics spacing

type Glyphs = Map Char Glyph

glyphsBy :: State Glyphs () -> Glyphs
glyphsBy = flip execState Map.empty