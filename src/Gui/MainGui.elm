module Gui.MainGui where
{- Internal modules -}
import Gui.Helpers
import Gui.EditModeHelp as EditModeHelp
import State.EditModes as EditModes

gui width gd em ges lsf editField = flow down
  [gd
  ,Gui.Helpers.coloredHorizontalLine width black
  ,Gui.Helpers.horizontalLine width
  ,Gui.Helpers.horizontalLine width
  ,flow right [plainText <| "Edit mode: "++show em]
  ,editField
  ,Gui.Helpers.coloredHorizontalLine width black
  ,Gui.Helpers.horizontalLine width
  ,EditModeHelp.help
  ,Gui.Helpers.horizontalLine width
  ,toText ges.errors |> Text.color red |> text
  ,Gui.Helpers.horizontalLine width
  ,Gui.Helpers.horizontalLine width
  ,Gui.Helpers.horizontalLine width
  ,plainText "Graphical Elm is released under the AGPL v 3 or later license. Copyright (c) 2013 Timothy Hobbs"
  ,toText "The source can be found here." |> Text.link "https://github.com/timthelion/graphical-elm" |> Text.color blue |> underline |> text
  ,Gui.Helpers.horizontalLine width
  ,Gui.Helpers.horizontalLine width
  ,Gui.Helpers.horizontalLine width]
