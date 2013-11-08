module GraphEditorState where
import open Graph
import open Coordinates

type GraphEditorState =
 {selectedNode: Node
 ,errors: String
 ,misc: [String]
 ,graph: Graph
 ,levelizedGraph: [[Node]]}

defaultEditorState =
 {selectedNode   = emptyNode
 ,errors         = ""
 ,misc           = []
 ,graph          = sampleGraph
 ,levelizedGraph = []}

emptyEditorState = 
 {selectedNode   = emptyNode
 ,errors         = ""
 ,misc           = []
 ,graph          = []
 ,levelizedGraph = []}

{- Cursor movement -}

updateLocation: {x:Int,y:Int} -> GraphEditorState -> GraphEditorState
updateLocation arrs ges =
 let
  ourCoordinates = coordinates ges.levelizedGraph
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
  ourCoordinates = coordinates ges.levelizedGraph
  oldCoord = getCoord ges.selectedNode.name ourCoordinates
 in
 setSelectedNode (f ges) oldCoord

setSelectedNode: GraphEditorState -> {x:Int,y:Int} -> GraphEditorState
setSelectedNode ges coord =
 {ges|selectedNode <-
  let
   ourCoordinates = coordinates ges.levelizedGraph
   newSelectedNode = getNode coord ourCoordinates
  in
  if | newSelectedNode.name == "" -> ges.selectedNode
     | otherwise -> newSelectedNode}

updateGraphLevelization: GraphEditorState -> GraphEditorState
updateGraphLevelization ges =
 {ges|levelizedGraph<-levelizeGraph ges.graph}