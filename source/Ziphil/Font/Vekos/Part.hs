--


module Ziphil.Font.Vekos.Part
  ( partBowl
  , partTail
  , partLes
  , partTransphone
  )
where

import Diagrams.Prelude
import Ziphil.Font.Util
import Ziphil.Font.Vekos.Param


-- k, p, c, l, a などの文字に共通する丸い部分のパスを生成します。
partBowl :: Part
partBowl = partBowlOuter <> partBowlInner

partBowlOuter :: Part
partBowlOuter = pathFromTrail trail
  where
    width = bowlWidth
    height = mean
    segments =
      [ bezier3 $^ (0, 25) $^ (0, height / 2 + overshoot) $^ (width / 2, height / 2 + overshoot)
      , bezier3 $^ (0, 25) $^ (0, height / 2 + overshoot) $^ (-width / 2, height / 2 + overshoot) # reverseSegment
      , bezier3 $^ (0, -25) $^ (0, -height / 2 - overshoot) $^ (-width / 2, -height / 2 - overshoot)
      , bezier3 $^ (0, -25) $^ (0, -height / 2 - overshoot) $^ (width / 2, -height / 2 - overshoot) # reverseSegment
      ]
    trail = reverseTrail $ closeTrail $ fromSegments segments

partBowlInner :: Part
partBowlInner = pathFromTrailAt trail $. (weightX, 0)
  where
    width = bowlWidth - weightX * 2
    height = mean - weightY * 2
    segments =
      [ bezier3 $^ (0, 25) $^ (0, height / 2 + overshoot) $^ (width / 2, height / 2 + overshoot)
      , bezier3 $^ (0, 25) $^ (0, height / 2 + overshoot) $^ (-width / 2, height / 2 + overshoot) # reverseSegment
      , bezier3 $^ (0, -25) $^ (0, -height / 2 - overshoot) $^ (-width / 2, -height / 2 - overshoot)
      , bezier3 $^ (0, -25) $^ (0, -height / 2 - overshoot) $^ (width / 2, -height / 2 - overshoot) # reverseSegment
      ]
    trail = closeTrail $ fromSegments segments

-- k, p, c, l などの文字に共通するエックスハイトの上下に飛び出す部分のパスを生成します。
partTail :: Part
partTail = pathFromTrail trail
  where
    bend = bowlWidth / 2
    height = mean / 2 + descender
    segments =
      [ bezier3 $^ (0, -250) $^ (-bend, -height + 200) $^ (-bend, -height)
      , straight $^ (weightX, 0)
      , bezier3 $^ (0, -250) $^ (-bend, -height + 200) $^ (-bend, -height) # reverseSegment
      , straight $^ (weightX, 0) # reverseSegment
      ]
    trail = closeTrail $ fromSegments segments
  
partLes :: Part
partLes = mconcat parts
  where
    parts =
      [ partBowl
      , partTail # translate $^ (bowlWidth - weightX, 0)
      ]

weightTransphoneX :: Double
weightTransphoneX = weightX * 0.95

-- g, b などの文字に共通する変音符部分のパスを生成します。
partTransphone :: Part
partTransphone = pathFromTrail trail
  where
    bend = 50
    height = mean
    segments = 
      [ bezier3 $^ (0, 0) $^ (bend, -height / 2 + 100) $^ (bend, -height / 2)
      , bezier3 $^ (0, 0) $^ (bend, height / 2 - 100) $^ (bend, height / 2) # reverseSegment
      , straight $^ (weightTransphoneX, 0)
      , bezier3 $^ (0, 0) $^ (bend, height / 2 - 100) $^ (bend, height / 2)
      , bezier3 $^ (0, 0) $^ (bend, -height / 2 + 100) $^ (bend, -height / 2) # reverseSegment
      , straight $^ (weightTransphoneX, 0) # reverseSegment
      ]
    trail = closeTrail $ fromSegments segments