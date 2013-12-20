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
    ,Graphics.Input.FieldState -> Element)
editFieldBuilder em ges =
 case em of
  EditModes.Code -> codeField ges
  EditModes.Name -> nameField ges
  EditModes.Parents -> parentsField ges
  EditModes.Delete -> deleteField ges
  EditModes.Explore -> emptyField
  EditModes.CodeView -> emptyField
  EditModes.SaveOpen -> saveOpenField ges
  EditModes.AddNode -> addNodeField ges
  EditModes.Misc -> miscField ges
  EditModes.TypeView -> emptyField
  EditModes.Type -> typeField ges

codeField: EditorState.EditorState ->  (Graphics.Input.FieldState, Graphics.Input.FieldState -> Element)
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
  ,(\fs->
   flow right
   <|(editorFields.field
         makeEvent
         "Enter code here."
         fs)
   :: plainText "Language:"
   :: asText node.value.language
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
  ,(\fs->
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
 ,(\fs->
  editorFields.field
   makeEvent
   (join "," node.parents)
   fs))

deleteField ges =
 let
  node = ges.selectedNode
 in
 ignoreFieldState <|
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
  ,(\fs-> saveOpenField fs))

addNodeField ges =
   let
    addNodeEvent fs = (fs,EditorEvents.SetEventRegister <| (\en->EventRegisters.AddNode {en|name<-fs.string}) Graph.emptyNode)
    addNodeField fs = editorFields.field addNodeEvent "Add node" fs
   in
   (Graphics.Input.emptyFieldState
   ,(\fs->
   flow right [addNodeField fs, plainText "Press enter to add"]))

miscField ges =
 let 
  emptyFieldState = Graphics.Input.emptyFieldState
  setMiscEvent fs = (fs,EditorEvents.SetMisc fs.string)
 in
 ({emptyFieldState|string<-ges.misc}
 ,(\fs->
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
 ,\fs->editorFields.field makeEvent currentType fs)

{------------------------------------------}

ignoreFieldState element = (emptyFieldState ,(\fs->element))
emptyField = ignoreFieldState <| plainText ""

editField
 :  (Graphics.Input.FieldState,Graphics.Input.FieldState->Element)
 -> Graphics.Input.FieldState
 -> WhatChanged.EventSource
 -> Element
editField (initFS,efb) efs changed =
 case changed of
  WhatChanged.A -> efb initFS
  WhatChanged.B -> efb efs

{- Field/button initializers -}

emptyFieldState = Graphics.Input.emptyFieldState

editorFields = Graphics.Input.fields (Graphics.Input.emptyFieldState,EditorEvents.NoEvent)

editorFieldsMultiline = Graphics.Input.fieldsMultiline (Graphics.Input.emptyFieldState,EditorEvents.NoEvent)

fieldStates (fs,_) = fs

editorFieldEvents (_,ev) = ev 

editorButtons = Graphics.Input.buttons EditorEvents.NoEvent

editButtonEvents = editorButtons.events
