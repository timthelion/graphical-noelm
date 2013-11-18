{-

This module provides some simple gui elements that didn't make their way into the standard libs.  It could probably be made into it's own library.

-}
module Gui.Helpers where
{- Standard lib imports -}
import Text

horizontalLine width = coloredHorizontalLine width grey

coloredHorizontalLine width c =
 collage width 2
  [  traced (solid c)
  <| segment
      (-(toFloat width/2),0)
      (toFloat width/2,0)]

verticalLine = toText "|" |> Text.color grey |> text
