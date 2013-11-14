{-
This module is the main module for the program.

It handles drawing of all GUI components.

It hands out the task of sorting and levelizing graphs to the Graph module.
The main types are also found in the Graph module.

The Coordinates module is a simple helper module to find the indexes of an item in a 2D list.

The CodeGenerator module handles BOTH parsing and code generation.

copyright (c) 2013 Timothy Hobbs
Released under the terms of the GNU AGPL v 3 or later.
See more licence information at the end of the file, and or in the file COPYING.
-}
module GraphicalElm where
import open List
import Keyboard
import Keyboard.Keys
import Window
import Graphics.Input
import Either
import String

import Graph
import open Coordinates
import open CodeGenerator
import GraphEditorState
import GraphEditorEvents
import GraphEditorStateMachine
import open EditModes
import EditModeHelp

movement = merge ctrlArrows hjklMovement

ctrlArrows =
 (\arrs->GraphEditorEvents.Arrows arrs)
 <~
  keepWhen
   Keyboard.ctrl
   {x=0,y=0}
   Keyboard.arrows

hjklMovement =
 (\arrs-> GraphEditorEvents.Arrows arrs)
 <~ Keyboard.directionKeys Keyboard.Keys.j Keyboard.Keys.k Keyboard.Keys.h Keyboard.Keys.l

graphEditorState = GraphEditorStateMachine.graphEditorState <| merges
   [sampleOn applyKeyPress graphEditorFields.events
   ,graphEditorButtons.events
   ,sampleOn loadSavedKeyPress loadSavedFields.events
   ,movement]

graphEditorFields = Graphics.Input.fields GraphEditorEvents.NoEvent

applyKeyPress = keepIf id False <| (Keyboard.isKeyDown Keyboard.Keys.enter)

graphEditorButtons = Graphics.Input.buttons GraphEditorEvents.NoEvent

editField: EditMode -> Graph.Node -> Element
editField em node =
 let
  emptyFieldState=Graphics.Input.emptyFieldState
 in
 case em of
  Code ->
   let
    makeEvent fs =
     let
      value = node.value
     in
     GraphEditorEvents.Replace {node|value<-{value|code<-fs.string}}
    makeLangEvent lang=
     let
      value = node.value
     in
     GraphEditorEvents.Replace {node|value<-{value|language<-lang}}
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
  Name ->
   let
    makeEvent fs =
     GraphEditorEvents.Rename {oldName=node.name,newName=fs.string}
   in
    graphEditorFields.field
     makeEvent
     node.name
     {emptyFieldState|string<-node.name}
  Parents ->
   let
    makeEvent fs =
     let
      newParents=String.split "," fs.string
     in
     GraphEditorEvents.Replace {node|parents<-
                 if | fs.string=="" -> []
                    | otherwise -> newParents}
   in
    graphEditorFields.field
     makeEvent
     (join "," node.parents)
     {emptyFieldState|string<-(join "," node.parents)}
  Delete ->
   graphEditorButtons.button
    (GraphEditorEvents.DeleteEvent node)
    "Delete"
  Explore -> plainText ""
  CodeView -> plainText ""
  SaveCompile -> plainText ""
  GlobalAdd ->
   let
    addNodeField = graphEditorFields.field ((\en fs->GraphEditorEvents.AddNode {en|name<-fs.string})Graph.emptyNode) "Add node" Graphics.Input.emptyFieldState
    addMiscField = graphEditorFields.field (\fs->GraphEditorEvents.AddMisc fs.string) "Add misc(imports, type declarations, ect.)" Graphics.Input.emptyFieldState
   in
   flow down
    [addNodeField
    ,addMiscField]

editFieldS: Signal Element
editFieldS = (\em ges->editField em ges.selectedNode) <~ editMode ~ graphEditorState

displayNode: EditMode -> Graph.Node -> Graph.Node -> Element
displayNode mode selected node =
 let nodeString = case mode of
       CodeView -> node.value.code
       _ -> node.name
 in
 if | node.name==selected.name ->
        let nodeText: Text
            nodeText = monospace <| toText (String.cons '*' nodeString)
            coloredText: Text
            coloredText = Text.color green nodeText
            selectedElm: Element
            selectedElm = text coloredText
        in
        selectedElm
    | any (\np->node.name==np) selected.parents -> toText nodeString |> Text.color red |> monospace |> text
    | any (\np->selected.name==np) node.parents ->  toText nodeString |> Text.color blue |> monospace |> text
    | otherwise -> text <|monospace <| toText nodeString

horizontalLine width = coloredHorizontalLine width grey

coloredHorizontalLine width c =
 collage width 2
  [  traced (solid c)
  <| segment
      (-(toFloat width/2),0)
      (toFloat width/2,0)]

verticalLine = toText "|" |> Text.color grey |> text

graphDisplay =
 (\ges em width ->
   flow down
    <| intersperse (horizontalLine width)
    <| map (flow right)
    <| map (\level->
        intersperse verticalLine
        <| map (displayNode em ges.selectedNode) level)
    <| ges.levelizedGraph)
 <~ graphEditorState ~ editMode ~ Window.width

{- load saved -}

loadSavedKeyPress = keepIf id False <| Keyboard.isKeyDown Keyboard.Keys.f2

loadSavedFields = Graphics.Input.fields GraphEditorEvents.NoEvent

loadSavedField =
 (\_->
  loadSavedFields.field
  (\fs->
    let
     parsedM = parseSavedGraph fs.string
    in
    case parsedM of
     Either.Right ges -> GraphEditorEvents.SetState ges 
     Either.Left err -> GraphEditorEvents.ParseError err
     )
  "Paste code to load here"
  Graphics.Input.emptyFieldState) <~ loadSavedKeyPress

gui = (\width gd em ges lsf editField -> flow down
  [gd
  ,coloredHorizontalLine width black
  ,horizontalLine width
  ,horizontalLine width
  ,flow right [plainText <| "Edit mode: "++show em]
  ,flow right [editField,verticalLine,plainText "Press enter to apply changes."]
  ,coloredHorizontalLine width black
  ,horizontalLine width
  ,EditModeHelp.help
  ,horizontalLine width
  ,plainText "In order to load a saved graph; paste generated code here and then press the Home key and F2 to load."
  ,lsf
  ,toText ges.errors |> Text.color red |> text
  ,horizontalLine width
  ,horizontalLine width
  ,horizontalLine width
  ,plainText "Graphical Elm is released under the AGPL v 3 or later license. Copyright (c) 2013 Timothy Hobbs"
  ,toText "The source can be found here." |> Text.link "https://github.com/timthelion/graphical-elm" |> Text.color blue |> underline |> text  ,horizontalLine width
  ,horizontalLine width
  ,horizontalLine width
  ,case em of
    SaveCompile -> plainText <| generateCode ges
    _ -> plainText ""])
 <~ Window.width ~ graphDisplay ~ editMode ~ graphEditorState ~ loadSavedField ~ editFieldS

main = gui

{-
Graphical ELM - A program for editing graphs as graphs.
Visually, Architecturally

    Copyright (C) 2013  Timothy Hobbs <timothyhobbs@seznam.cz> thobbs.cz

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
