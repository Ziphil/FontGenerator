{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.FontGen.Util.Text
  ( ToText (..)
  , tshow
  , sub
  )
where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as Text


class ToText a where
  pack :: a -> Text

instance ToText String where
  pack = Text.pack

instance ToText Text where
  pack = id

tshow :: Show a => a -> Text
tshow = Text.pack . show

-- テキスト中の与えられた文字列に一致する箇所を変換するためのセッターです。
sub :: (ToText a, Show b) => a -> Setter Text Text a b
sub needle = sets $ \func -> Text.replace (pack needle) (tshow $ func needle)