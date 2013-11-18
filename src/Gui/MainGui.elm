module Gui.MainGui where
{- Internal modules -}
import Gui.Helpers
import Gui.EditModeHelp as EditModeHelp
import State.EditModes as EditModes
import ParserAndCompiler.CodeGenerator as CodeGenerator

gui width gd em ges lsf editField = flow down
  [gd
  ,Gui.Helpers.coloredHorizontalLine width black
  ,Gui.Helpers.horizontalLine width
  ,Gui.Helpers.horizontalLine width
  ,flow right [plainText <| "Edit mode: "++show em]
  ,flow right [editField,Gui.Helpers.verticalLine,plainText "Press enter to apply changes."]
  ,Gui.Helpers.coloredHorizontalLine width black
  ,Gui.Helpers.horizontalLine width
  ,EditModeHelp.help
  ,Gui.Helpers.horizontalLine width
  ,plainText "In order to load a saved graph; paste generated code here and then press the Home key and F2 to load."
  ,lsf
  ,toText ges.errors |> Text.color red |> text
  ,Gui.Helpers.horizontalLine width
  ,Gui.Helpers.horizontalLine width
  ,Gui.Helpers.horizontalLine width
  ,plainText "Graphical Elm is released under the AGPL v 3 or later license. Copyright (c) 2013 Timothy Hobbs"
  ,toText "The source can be found here." |> Text.link "https://github.com/timthelion/graphical-elm" |> Text.color blue |> underline |> text
  ,Gui.Helpers.horizontalLine width
  ,Gui.Helpers.horizontalLine width
  ,Gui.Helpers.horizontalLine width
  ,case em of
    EditModes.SaveCompile -> plainText <| CodeGenerator.generateCode ges
    _ -> plainText ""]
