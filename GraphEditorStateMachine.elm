{-
This module holds most of the state for the graphical elm editor.  Other state can be found in the EditModes module.

See more licence information at the end of the file, and or in the file COPYING.
-}
module GraphEditorStateMachine where
import Graph
import GraphEditorEvents
import GraphEditorState
import Coordinates

graphEditorState: Signal GraphEditorEvents.GraphEditorEvent -> Signal GraphEditorState.GraphEditorState
graphEditorState events =
 dropRepeats
 <| foldp
  (\fieldEventM ges ->
   case fieldEventM of
    GraphEditorEvents.NoEvent          -> ges

    GraphEditorEvents.Arrows arrs      -> updateLocation arrs ges

    GraphEditorEvents.Replace node     -> restoreCoordinates ges (\ges->{ges|graph <- Graph.replaceNode node ges.graph}) |> updateGraphLevelization
    GraphEditorEvents.AddNode node     -> restoreCoordinates ges (\ges->{ges|graph <- Graph.addNode node ges.graph}) |> updateGraphLevelization
    GraphEditorEvents.AddMisc misc     -> restoreCoordinates ges (\ges->{ges|misc <- ges.misc ++ [misc]})
    GraphEditorEvents.DeleteEvent node -> restoreCoordinates ges (\ges->{ges|graph <- Graph.deleteNode node.name ges.graph}) |> updateGraphLevelization
    GraphEditorEvents.Rename rename    -> restoreCoordinates ges (\ges->{ges|graph <- Graph.renameNode rename.oldName rename.newName ges.graph}) |> updateGraphLevelization

    GraphEditorEvents.SetState ges  -> ges |> updateGraphLevelization
    GraphEditorEvents.ParseError err   -> {ges|errors<-err}
    )
  GraphEditorState.defaultEditorState
  events


{- Cursor movement -}

updateLocation: {x:Int,y:Int} -> GraphEditorState.GraphEditorState -> GraphEditorState.GraphEditorState
updateLocation arrs ges =
 let
  ourCoordinates = Coordinates.coordinates ges.levelizedGraph
  moved = arrs.x/=0|| arrs.y/=0
 in
 if | moved ->
     let
      oldCoord =
       Coordinates.getCoord ges.selectedNode.name ourCoordinates
      newCoord =
       {x = max ( arrs.x + oldCoord.x ) 0
       ,y = max ( arrs.y + oldCoord.y ) 0}
     in setSelectedNode ges newCoord
    | otherwise -> ges

restoreCoordinates: GraphEditorState.GraphEditorState -> (GraphEditorState.GraphEditorState -> GraphEditorState.GraphEditorState) -> GraphEditorState.GraphEditorState
restoreCoordinates ges f =
 let
  ourCoordinates = Coordinates.coordinates ges.levelizedGraph
  oldCoord = Coordinates.getCoord ges.selectedNode.name ourCoordinates
 in
 setSelectedNode (f ges) oldCoord

setSelectedNode: GraphEditorState.GraphEditorState -> {x:Int,y:Int} -> GraphEditorState.GraphEditorState
setSelectedNode ges coord =
 {ges|selectedNode <-
  let
   ourCoordinates = Coordinates.coordinates ges.levelizedGraph
   newSelectedNode = Coordinates.getNode coord ourCoordinates
  in
  if | newSelectedNode.name == "" -> ges.selectedNode
     | otherwise -> newSelectedNode}

updateGraphLevelization: GraphEditorState.GraphEditorState -> GraphEditorState.GraphEditorState
updateGraphLevelization ges =
 {ges|levelizedGraph<-Graph.levelizeGraph ges.graph}

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
