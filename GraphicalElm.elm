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
import Window
import Graphics.Input
import Either
import String

import open Graph
import open Coordinates
import open CodeGenerator

data EditMode = Code | Language | Name | Parents | Delete | Explore | SaveCompile

editMode =
 dropRepeats
 <| foldp
  (\down oldMode->
   if down
    then
     case oldMode of
      Code -> Language
      Language -> Name
      Name -> Parents
      Parents -> Delete
      Delete -> Explore
      Explore -> SaveCompile
      SaveCompile -> Code
    else oldMode)
  Explore
  <| Keyboard.isDown 115

sampleGraph =
 [{parents=[],name="main",value=defaultValue}
 ]

type GraphEditorState =
 {selectedNode: Node
 ,errors: String
 ,graph: Graph}

updateLocation: {x:Int,y:Int} -> GraphEditorState -> GraphEditorState
updateLocation arrs ges =
 let
  ourCoordinates = coordinates ges.graph
  moved = arrs.x/=0|| arrs.y/=0
 in
 if | moved ->
     let
      oldCoord =
       getCoord ges.selectedNode.name ourCoordinates
      newCoord =
       {x = max ( arrs.x + oldCoord.x ) 0
       ,y = max ( arrs.y + oldCoord.y ) 0}
     in setSelectedNode ges newCoord
    | otherwise -> ges

restoreCoordinates: GraphEditorState -> (GraphEditorState -> GraphEditorState) -> GraphEditorState
restoreCoordinates ges f =
 let
  ourCoordinates = coordinates ges.graph
  oldCoord = getCoord ges.selectedNode.name ourCoordinates
 in
 setSelectedNode (f ges) oldCoord

setSelectedNode: GraphEditorState -> {x:Int,y:Int} -> GraphEditorState
setSelectedNode ges coord =
 {ges|selectedNode<-
  let
   ourCoordinates = coordinates ges.graph
   newSelectedNode = getNode coord ourCoordinates
  in
  if | newSelectedNode.name == "" -> ges.selectedNode
     | otherwise -> newSelectedNode}

defaultEditorState = {selectedNode=emptyNode,errors="",graph=sampleGraph}

ctrlArrows =
 dropRepeats
 <| (\arrs->Arrows arrs)
 <~
  keepWhen
   Keyboard.ctrl
   {x=0,y=0}
   Keyboard.arrows

graphEditorFields = Graphics.Input.fields NoEvent

applyKeyPress = keepIf id False <| (Keyboard.isDown 13)

graphEditorButtons = Graphics.Input.buttons NoEvent

data GraphEditorEvent
 = NoEvent
 | Replace Node
 | OpenGraph Graph
 | ParseError String
 | Add Node
 | DeleteEvent Node
 | Rename {oldName: String, newName: String}
 | Arrows {x:Int,y:Int}

graphEditorState =
 dropRepeats
 <| foldp
  (\fieldEventM ges ->
   case fieldEventM of
    NoEvent          -> ges

    Arrows arrs      -> updateLocation arrs ges

    Replace node     -> restoreCoordinates ges (\ges->{ges|graph <- replaceNode node ges.graph})
    Add node         -> restoreCoordinates ges (\ges->{ges|graph <- addNode node ges.graph})
    DeleteEvent node -> restoreCoordinates ges (\ges->{ges|graph <- deleteNode node.name ges.graph})
    Rename rename    -> restoreCoordinates ges (\ges->{ges|graph <- renameNode rename.oldName rename.newName ges.graph})

    OpenGraph graph  -> {defaultEditorState|graph<-graph}
    ParseError err   -> {ges|errors<-err}
    )
  defaultEditorState
  <| merges
   [sampleOn applyKeyPress graphEditorFields.events
   ,graphEditorButtons.events
   ,sampleOn addNodeKeyPress addNodeFields.events
   ,sampleOn loadSavedKeyPress loadSavedFields.events
   ,ctrlArrows]


editField: EditMode -> Node -> Element
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
     Replace {node|value<-{value|code<-fs.string}}
   in
    graphEditorFields.field
     makeEvent
     node.value.code
     {emptyFieldState|string<-node.value.code}
  Language ->
   let
    makeEvent lang=
     let
      value = node.value
     in
     Replace {node|value<-{value|language<-lang}}
   in
    flow right <| asText node.value.language ::
     map
     (\lang ->
      graphEditorButtons.button
       (makeEvent lang)
       (show lang))
     [ElmLang,Ikcilpazc]
  Name ->
   let
    makeEvent fs =
     Rename {oldName=node.name,newName=fs.string}
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
     Replace {node|parents<-
                 if | fs.string=="" -> []
                    | otherwise -> newParents}
   in
    graphEditorFields.field
     makeEvent
     (join "," node.parents)
     {emptyFieldState|string<-(join "," node.parents)}
  Delete ->
   graphEditorButtons.button
    (DeleteEvent node)
    "Delete"
  Explore -> plainText ""
  SaveCompile -> plainText ""

editFieldS: Signal Element
editFieldS = (\em ges->editField em ges.selectedNode) <~ editMode ~ graphEditorState

displayNode: Node -> Node -> Element
displayNode selected node =
 if | node.name==selected.name ->
        let nameText: Text
            nameText = monospace <| toText (String.cons '*' node.name)
            coloredText: Text
            coloredText = Text.color green nameText
            selectedElm: Element
            selectedElm = text coloredText
        in
        selectedElm
    | any (\np->node.name==np) selected.parents -> toText node.name |> Text.color red |> monospace |> text
    | any (\np->selected.name==np) node.parents ->  toText node.name |> Text.color blue |> monospace |> text
    | otherwise -> text <|monospace <| toText node.name

horizontalLine width = coloredHorizontalLine width grey

coloredHorizontalLine width c =
 collage width 2
  [  traced (solid c)
  <| segment
      (-(toFloat width/2),0)
      (toFloat width/2,0)]

verticalLine = toText "|" |> Text.color grey |> text

graphDisplay =
 (\ges width ->
   flow down
    <| intersperse (horizontalLine width)
    <| map (flow right)
    <| map (\level->
        intersperse verticalLine
        <| map (displayNode ges.selectedNode) level)
    <| levelizeGraph ges.graph)
 <~ graphEditorState ~ Window.width

{- add Node -}

addNodeKeyPress = keepIf id False <| Keyboard.isDown 120

addNodeFields = Graphics.Input.fields NoEvent

addNodeField = (\_->addNodeFields.field (\fs->Add {emptyNode|name<-fs.string}) "Add node" Graphics.Input.emptyFieldState) <~ addNodeKeyPress

{- load saved -}

loadSavedKeyPress = keepIf id False <| Keyboard.isDown 113

loadSavedFields = Graphics.Input.fields NoEvent

loadSavedField =
 (\_->
  loadSavedFields.field
  (\fs->
    let
     parsedM = parseSavedGraph fs.string
    in
    case parsedM of
     Either.Right graph -> OpenGraph graph
     Either.Left err -> ParseError err
     )
  "Paste code to load here"
  Graphics.Input.emptyFieldState) <~ loadSavedKeyPress

main =
 (\width gd em anf ges lsf ef -> flow down
  [gd
  ,coloredHorizontalLine width black
  ,horizontalLine width
  ,horizontalLine width
  ,flow right [plainText <| "Edit mode: "++show em,verticalLine,plainText <| "Press F4 to change modes."]
  ,flow right [verticalLine,ef,plainText "Press enter to apply changes."]
  ,coloredHorizontalLine width black
  ,horizontalLine width
  ,horizontalLine width
  ,flow right [anf,verticalLine,plainText "Press F9 to add node."]
  ,horizontalLine width
  ,horizontalLine width
  ,horizontalLine width
  ,horizontalLine width
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
    SaveCompile -> plainText <| generateCode ges.graph
    _ -> plainText ""])
 <~ Window.width ~ graphDisplay ~ editMode ~ addNodeField ~ graphEditorState ~ loadSavedField ~ editFieldS
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