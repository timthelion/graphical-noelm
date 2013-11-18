{-

This module provides the edit fields that you see at the bottom of the graph.

-}
module Gui.EditFields where
{- Standard lib imports -}
import Graphics.Input
import String

{- Internal modules -}
import State.EditModes as EditModes
import State.EditorEvents as EditorEvents
import State.EditorState as EditorState
import LevelizedGraphs.Graph as Graph


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
   <|(graphEditorFields.field
         makeEvent
         node.value.code
         {emptyFieldState|string<-node.value.code})
   :: plainText "Language:"
   :: asText node.value.language
   :: (map
       (\lang ->
        graphEditorButtons.button
         (makeLangEvent lang)
         (show lang))
       [Graph.ElmLang,Graph.Ikcilpazc])
  EditModes.Name ->
   let
    makeEvent fs =
     EditorEvents.Rename {oldName=node.name,newName=fs.string}
   in
    graphEditorFields.field
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
    graphEditorFields.field
     makeEvent
     (join "," node.parents)
     {emptyFieldState|string<-(join "," node.parents)}
  EditModes.Delete ->
   graphEditorButtons.button
    (EditorEvents.DeleteEvent node)
    "Delete"
  EditModes.Explore -> plainText ""
  EditModes.CodeView -> plainText ""
  EditModes.SaveCompile -> plainText ""
  EditModes.GlobalAdd ->
   let
    addNodeField = graphEditorFields.field ((\en fs->EditorEvents.AddNode {en|name<-fs.string})Graph.emptyNode) "Add node" Graphics.Input.emptyFieldState
    addMiscField = graphEditorFields.field (\fs->EditorEvents.AddMisc fs.string) "Add misc(imports, type declarations, ect.)" Graphics.Input.emptyFieldState
   in
   flow down
    <| [addNodeField
       ,addMiscField] ++ map plainText ges.misc

{- Field/button initializers -}

graphEditorFields = Graphics.Input.fields EditorEvents.NoEvent

graphEditorButtons = Graphics.Input.buttons EditorEvents.NoEvent

editFieldApply applyKeyPress =
 sampleOn
  applyKeyPress
  graphEditorFields.events

editButtonEvents = graphEditorButtons.events
