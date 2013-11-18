{-

This module provides the edit fields that you see at the bottom of the graph.

-}
module Gui.EditFields where
{- Standard lib imports -}
import Graphics.Input
import Graphics.Element
import String
import Either

{- Internal modules -}
import State.EditModes as EditModes
import State.EditorEvents as EditorEvents
import State.EditorState as EditorState
import LevelizedGraphs.Graph as Graph
import ParserAndCompiler.CodeGenerator as CodeGenerator


editField: EditModes.EditMode -> EditorState.EditorState -> Element
editField em ges =
 let
  node = ges.selectedNode
  emptyFieldState=Graphics.Input.emptyFieldState
 in
 case em of
  EditModes.Code ->
   let
    makeEvent fs =
     let
      value = node.value
     in
     EditorEvents.Replace {node|value<-{value|code<-fs.string}}
    makeLangEvent lang=
     let
      value = node.value
     in
     EditorEvents.Replace {node|value<-{value|language<-lang}}
   in
   flow right
   <|(editorFields.field
         makeEvent
         node.value.code
         {emptyFieldState|string<-node.value.code})
   :: plainText "Language:"
   :: asText node.value.language
   :: (map
       (\lang ->
        editorButtons.button
         (makeLangEvent lang)
         (show lang))
       [Graph.ElmLang,Graph.Ikcilpazc])
  EditModes.Name ->
   let
    makeEvent fs =
     EditorEvents.Rename {oldName=node.name,newName=fs.string}
   in
    editorFields.field
     makeEvent
     node.name
     {emptyFieldState|string<-node.name}
  EditModes.Parents ->
   let
    makeEvent fs =
     let
      newParents=String.split "," fs.string
     in
     EditorEvents.Replace {node|parents<-
                 if | fs.string=="" -> []
                    | otherwise -> newParents}
   in
    editorFields.field
     makeEvent
     (join "," node.parents)
     {emptyFieldState|string<-(join "," node.parents)}
  EditModes.Delete ->
   editorButtons.button
    (EditorEvents.DeleteEvent node)
    "Delete"
  EditModes.Explore -> plainText ""
  EditModes.CodeView -> plainText ""
  EditModes.SaveOpen ->
    let
     emptyFieldState = Graphics.Input.emptyFieldState
     setStateEvent fs =
      case CodeGenerator.parseSavedGraph fs.string of
       Either.Left err -> EditorEvents.ParseError err
       Either.Right newState -> EditorEvents.SetState newState
     saveOpenField = editorFieldsMultiline.field setStateEvent "Paste code here to load it." {emptyFieldState|string<-CodeGenerator.generateCode ges}
    in
    saveOpenField
  EditModes.AddNode ->
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
    addMiscField

{- Field/button initializers -}

editorFields = Graphics.Input.fields EditorEvents.NoEvent

editorFieldsMultiline = Graphics.Input.fieldsMultiline EditorEvents.NoEvent

editorButtons = Graphics.Input.buttons EditorEvents.NoEvent

editButtonEvents = editorButtons.events
