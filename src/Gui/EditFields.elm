{-

This module provides the edit fields that you see at the bottom of the graph.

-}
module Gui.EditFields where
{- Standard lib imports -}
import Graphics.Input
import Graphics.Element
import String
import Either

{- External libraries -}
import Signal.WhatChanged as WhatChanged

{- Internal modules -}
import State.EditModes as EditModes
import State.EditorEvents as EditorEvents
import State.EditorState as EditorState
import State.EventRegisters as EventRegisters
import LevelizedGraphs.Graph as Graph
import ParserAndCompiler.Compiler as Compiler
import ParserAndCompiler.Parser as Parser

editFieldBuilder
 :  EditModes.EditMode
 -> EditorState.EditorState
 -> (Graphics.Input.FieldState
    ,Graphics.Input.FieldState -> EditorState.EditorState -> Element)
editFieldBuilder em =
 case em of
  {- move modes -}
  EditModes.Explore -> emptyField
  EditModes.CodeView -> infoField
  EditModes.TypeView -> infoField

  {- insert modes -}
   {- node editing -}
  EditModes.Name -> nameField
  EditModes.Parents -> parentsField
  EditModes.Type -> typeField
  EditModes.Code -> codeField
  EditModes.Delete -> deleteField
   {- global editing -}
  EditModes.SaveOpen -> saveOpenField
  EditModes.AddNode -> addNodeField
  EditModes.Misc -> miscField


codeField: EditorState.EditorState ->  (Graphics.Input.FieldState, Graphics.Input.FieldState -> EditorState.EditorState -> Element)
codeField ges =
 let
  node = ges.selectedNode
  value = node.value
  makeEvent: Graphics.Input.FieldState -> (Graphics.Input.FieldState,EditorEvents.EditorEvent)
  makeEvent fs =
   (fs,EditorEvents.Replace {node|value<-{value|code<-fs.string}})
  makeLangEvent lang =
   EditorEvents.Replace {node|value<-{value|language<-lang}}
 in
  ({emptyFieldState|string<-node.value.code}
  ,(\fs ges' ->
   flow right
   <|(editorFields.field
         makeEvent
         "Enter code here."
         fs)
   :: plainText "Language:"
   :: asText ges'.selectedNode.value.language
   :: (map
       (\lang ->
        editorButtons.button
         (makeLangEvent lang)
         (show lang))
       Graph.languages)))

nameField ges =
 let
  node = ges.selectedNode
  makeEvent fs =
   (fs,EditorEvents.SetEventRegister <| EventRegisters.Rename {oldName=node.name,newName=fs.string})
 in
  ({emptyFieldState|string<-node.name}
  ,(\fs ges' ->
    editorFields.field
     makeEvent
     node.name
     fs))

parentsField ges =
 let
  node = ges.selectedNode
  makeEvent fs =
   let
    newParents=String.split "," fs.string
   in
   (fs
   ,EditorEvents.Replace {node|parents<-
               if | fs.string=="" -> []
                  | otherwise -> newParents})
 in
 ({emptyFieldState|string<-(join "," node.parents)}
 ,(\fs ges' ->
  editorFields.field
   makeEvent
   (join "," node.parents)
   fs))

deleteField ges =
 let
  node = ges.selectedNode
 in
 ignoreStateUpdates <|
 editorButtons.button
  (EditorEvents.DeleteEvent node)
  "Delete"

saveOpenField ges =
 let
  node = ges.selectedNode
  setStateEvent fs =
   case Parser.parseSavedGraph fs.string of
    Either.Left err -> (fs,EditorEvents.ParseError err)
    Either.Right newState -> (fs,EditorEvents.SetState newState)
  saveOpenField fs = editorFieldsMultiline.field setStateEvent "Paste code here to load it." fs
 in
 ({emptyFieldState|string<-Compiler.generateCode ges}
  ,(\fs ges' -> saveOpenField fs))

addNodeField ges =
   let
    addNodeEvent fs = (fs,EditorEvents.SetEventRegister <| (\en->EventRegisters.AddNode {en|name<-fs.string}) Graph.emptyNode)
    addNodeField fs = editorFields.field addNodeEvent "Add node" fs
   in
   (Graphics.Input.emptyFieldState
   ,(\fs ges' ->
   flow right [addNodeField fs, plainText "Press enter to add"]))

miscField ges =
 let 
  emptyFieldState = Graphics.Input.emptyFieldState
  setMiscEvent fs = (fs,EditorEvents.SetMisc fs.string)
 in
 ({emptyFieldState|string<-ges.misc}
 ,(\fs ges' ->
 editorFieldsMultiline.field
  setMiscEvent
  "Add misc(imports, type declarations, ect.)"
  fs
 |> Graphics.Element.size 500 400))

typeField ges =
 let
  node = ges.selectedNode
  value = node.value
  currentType =
   case node.value.ntype of
    Just ntype -> ntype
    Nothing -> ""
  newType fs = String.trim fs.string
  makeEvent fs =
   (fs
   ,EditorEvents.Replace
     {node|value <-{value|ntype<-
       if | newType fs == "" -> Nothing
          | otherwise -> Just <| newType fs}})
 in
 ({emptyFieldState|string<-currentType}
 ,\fs ges' ->editorFields.field makeEvent currentType fs)

{------------------------------------------}

ignoreStateUpdates element = (emptyFieldState ,(\fs ges'->element))
emptyField ges =  ignoreStateUpdates <| plainText ""
infoField ges = ignoreStateUpdates <|
 flow down
  [flow right [plainText "Name:",plainText ges.selectedNode.name]
  ,flow right [plainText "Parents:",plainText <| String.concat <| intersperse "," ges.selectedNode.parents]
  ,flow right [plainText "Type:",plainText
   <| case ges.selectedNode.value.ntype of
       Just ntype -> ntype
       Nothing -> "No type entered"]
  ,flow right [plainText "Code:",plainText ges.selectedNode.value.code]
  ,flow right [plainText "Language:",plainText <| show ges.selectedNode.value.language]]

type FieldAndEditorStateUpdate =
 {fieldState: Graphics.Input.FieldState
 ,editorState: EditorState.EditorState}

editField
 :  (Graphics.Input.FieldState,Graphics.Input.FieldState-> EditorState.EditorState->Element)
 -> FieldAndEditorStateUpdate
 -> WhatChanged.EventSource
 -> Element
editField (initFS,efb) updates changed =
 case changed of
  WhatChanged.A -> efb initFS updates.editorState
  WhatChanged.B -> efb updates.fieldState updates.editorState

{- Field/button initializers -}

emptyFieldState = Graphics.Input.emptyFieldState

editorFields = Graphics.Input.fields (Graphics.Input.emptyFieldState,EditorEvents.NoEvent)

editorFieldsMultiline = Graphics.Input.fieldsMultiline (Graphics.Input.emptyFieldState,EditorEvents.NoEvent)

fieldStates (fs,_) = fs

editorFieldEvents (_,ev) = ev 

editorButtons = Graphics.Input.buttons EditorEvents.NoEvent

editButtonEvents = editorButtons.events
