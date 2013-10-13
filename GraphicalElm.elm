module GraphicalElm where
import open List
import Keyboard
import open Graph
import open Coordinates

sampleGraph =
 [{parents=[],name="arrows",value=defaultValue}
 ,{parents=["arrows"],name="loc",value=defaultValue}
 ,{parents=[],name="mouse",value=defaultValue}
 ,{parents=["loc","mouse"],name="main",value=defaultValue}
 ]

type GraphEditorState =
 {selectedNode: String
 ,graph: Graph}

graphEditorState =
 foldp
  (\arrs ges->
   let ourCoordinates = coordinates ges.graph in
   if | arrs.x/=0|| arrs.y/=0 ->
         let oldCoord=getCoord ges.selectedNode ourCoordinates
             newCoord={x=max (arrs.x+oldCoord.x) 0,y=max (-arrs.y+oldCoord.y) 0} in
         {ges|selectedNode<-
           let newSelectedNode =(getNode newCoord ourCoordinates).name in
           if | newSelectedNode == "" -> ges.selectedNode
              | otherwise -> newSelectedNode}
      | otherwise -> ges)
  {selectedNode="arrows",graph=sampleGraph} <| keepWhen Keyboard.ctrl {x=0,y=0} <| Keyboard.arrows

displayNode: String -> Node -> Element
displayNode selected node =
 let nameText: Text
     nameText = toText node.name
     coloredText: Text
     coloredText = Text.color red nameText
     selectedElm: Element
     selectedElm = text coloredText
 in
 if | node.name==selected -> selectedElm
    | otherwise -> plainText node.name

main = {-asText <| coordinates  sampleGraph -} (\ges->flow down <| map (flow right) <| map (\level->map (displayNode ges.selectedNode) level) <| reverse <|  levelizeGraph ges.graph) <~ graphEditorState
