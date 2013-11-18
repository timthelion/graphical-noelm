module Gui.LoadSaved where
{- Standard library imports -}
import Graphics.Input
import Either

{- Internal modules -}
import ParserAndCompiler.CodeGenerator
import State.EditorEvents

loadSavedFields = Graphics.Input.fields State.EditorEvents.NoEvent

loadSavedFieldEvents loadSavedKeyPress =
 sampleOn
  loadSavedKeyPress
  loadSavedFields.events

loadSaved fieldState =
    let
     parsedM = ParserAndCompiler.CodeGenerator.parseSavedGraph fieldState.string
    in
    case parsedM of
     Either.Right ges -> State.EditorEvents.SetState ges 
     Either.Left err -> State.EditorEvents.ParseError err

loadSavedField _ =
 loadSavedFields.field
  loadSaved
  "Paste code to load here"
  Graphics.Input.emptyFieldState