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
import LevelizedGraphs.Graph as Graph
import ParserAndCompiler.CodeGenerator as CodeGenerator


editFieldBuilder: EditModes.EditMode -> EditorState.EditorState -> (Graphics.Input.FieldState, Graphics.Input.FieldState -> Element)
editFieldBuilder em ges =
 let
  node = ges.selectedNode
  emptyFieldState=Graphics.Input.emptyFieldState
  ignoreFieldState element = (emptyFieldState ,(\fs->element))
 in
 case em of
  EditModes.Code ->
   let
    makeEvent: Graphics.Input.FieldState -> (Graphics.Input.FieldState,EditorEvents.EditorEvent)
    makeEvent fs =
     let
      value = node.value
     in
     (fs
     ,EditorEvents.Replace {node|value<-{value|code<-fs.string}})
    makeLangEvent lang=
     let
      value = node.value
     in
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
       [Graph.ElmLang,Graph.Ikcilpazc])))
  EditModes.Name ->
   let
    makeEvent fs =
     (fs
     ,EditorEvents.Rename {oldName=node.name,newName=fs.string})
   in
   ({emptyFieldState|string<-node.name}
   ,(\fs->
    editorFields.field
     makeEvent
     node.name
     fs))
  EditModes.Parents ->
   let
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
  EditModes.Delete ->
   ignoreFieldState <|
   editorButtons.button
    (EditorEvents.DeleteEvent node)
    "Delete"
  EditModes.Explore -> ignoreFieldState <| plainText ""
  EditModes.CodeView -> ignoreFieldState <| plainText ""
  EditModes.SaveOpen ->
    let
     setStateEvent fs =
      case CodeGenerator.parseSavedGraph fs.string of
       Either.Left err -> (fs,EditorEvents.ParseError err)
       Either.Right newState -> (fs,EditorEvents.SetState newState)
     saveOpenField fs = editorFieldsMultiline.field setStateEvent "Paste code here to load it." fs
    in
    ({emptyFieldState|string<-CodeGenerator.generateCode ges}
    ,(\fs-> saveOpenField fs))
{-  EditModes.AddNode ->
   let
    addNodeEvent fs = (\en->EditorEvents.AddNode {en|name<-fs.string}) Graph.emptyNode
    addNodeField = editorFields.field addNodeEvent "Add node" Graphics.Input.emptyFieldState
   in
   flow right [addNodeField, plainText "Press enter to add"]
  EditModes.Misc ->
    let 
     emptyFieldState = Graphics.Input.emptyFieldState
     setMiscEvent fs = EditorEvents.SetMisc fs.string
     addMiscField = editorFieldsMultiline.field setMiscEvent "Add misc(imports, type declarations, ect.)" {emptyFieldState|string<-ges.misc}
                    |> Graphics.Element.size 500 400

    in
    addMiscField-}

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

editorFields = Graphics.Input.fields (Graphics.Input.emptyFieldState,EditorEvents.NoEvent)

editorFieldsMultiline = Graphics.Input.fieldsMultiline (Graphics.Input.emptyFieldState,EditorEvents.NoEvent)

fieldStates (fs,_) = fs

editorFieldEvents (_,ev) = ev 

editorButtons = Graphics.Input.buttons EditorEvents.NoEvent

editButtonEvents = editorButtons.events
