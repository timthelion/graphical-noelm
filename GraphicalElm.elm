module GraphicalElm where
import open List
import Keyboard
import Window
import Graphics.Input

import open Graph
import open Coordinates

data EditMode = Code | Language | Name | Parents

editMode =
 foldp
  (\down oldMode->
   if down
    then
     case oldMode of
      Code -> Language
      Language -> Name
      Name -> Parents
      Parents -> Code
    else oldMode)
  Code
  <| Keyboard.isDown 123

sampleGraph =
 [{parents=[],name="main",value=defaultValue}
 ]

type GraphEditorState =
 {selectedNode: Node
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
       {x = max ( arrs.x + oldCoord.x) 0
       ,y = max (-arrs.y + oldCoord.y) 0}
     in
      {ges|selectedNode<-
       let
        newSelectedNode =getNode newCoord ourCoordinates
       in
       if | newSelectedNode.name == "" -> ges.selectedNode
          | otherwise -> newSelectedNode}
    | otherwise -> ges

defaultEditorState = {selectedNode=emptyNode,graph=sampleGraph}

ctrlArrows =
 keepWhen
  Keyboard.ctrl
  {x=0,y=0}
  Keyboard.arrows

graphEditorFields = Graphics.Input.fields Nothing
graphEditorButtons = Graphics.Input.buttons Nothing

graphEditorState =
 foldp
  (\(arrs,fieldEventM) ges ->
   updateLocation arrs ges |>
   (\ges'->
   case fieldEventM of
    Nothing -> ges'
    Just event -> {ges'|graph<- addOrReplaceNode event ges'.graph}))
  defaultEditorState
  <| (,) <~ ctrlArrows ~ merges [graphEditorFields.events,graphEditorButtons.events,sampleOn addNodeKeyPress addNodeFields.events]

displayNode: Node -> EditMode -> Node -> Element
displayNode selected em node =
 let nameText: Text
     nameText = toText ('*'::node.name)
     coloredText: Text
     coloredText = Text.color green nameText
     selectedElm: Element
     selectedElm = flow down [text coloredText,editField]


     editField: Element
     editField =
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
          Just {node|value<-{value|code<-fs.string}}
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
          Just {node|value<-{value|language<-lang}}
        in
         flow right <| asText node.value.language ::
          map
          (\lang ->
           graphEditorButtons.button
            (makeEvent lang)
            (show lang))
          [ElmLang,FooLang]
       Name -> plainText "TODO name"
        {-let
         makeEvent fs =
          Just {node|name<-fs.string}
        in
         graphEditorFields.field
          makeEvent
          node.name
          {emptyFieldState|string<-node.name}-}
       Parents ->
        let
         makeEvent fs =
          let
           newParents=split "," fs.string
          in
          Just {node|parents<-
                      if | fs.string=="" -> []
                         | otherwise -> newParents}
        in
         graphEditorFields.field
          makeEvent
          (join "," node.parents)
          {emptyFieldState|string<-(join "," node.parents)}
 in
 if | node.name==selected.name -> selectedElm
    | any (\np->node.name==np) selected.parents -> toText node.name |> Text.color red |> text
    | any (\np->selected.name==np) node.parents ->  toText node.name |> Text.color blue |> text
    | otherwise -> plainText node.name

graphDisplay =
 (\ges em  width height ->
   flow down
    <| intersperse (collage width 2 [traced (solid grey) <| segment (-(toFloat width/2),0) (toFloat width/2,0)])
    <| map (flow right)
    <| map (\level->
        intersperse (toText "|" |> Text.color grey |> text)
        <| map (displayNode ges.selectedNode em) level)
    <| reverse
    <| levelizeGraph ges.graph)
 <~ (sampleOn ctrlArrows graphEditorState) ~ editMode

addNodeKeyPress = keepIf id False <| Keyboard.isDown 120

addNodeFields = Graphics.Input.fields Nothing

addNodeField = (\_->addNodeFields.field (\fs->Just {emptyNode|name<-fs.string}) "Add node" Graphics.Input.emptyFieldState)<~ addNodeKeyPress

main =
 (\width height gd em ans-> flow down
  [gd width height
  ,plainText <| "Current edit mode "++show em++". Press F12 to change. Press F9 to add a node."
  ,ans])
 <~ Window.width ~ Window.height ~ graphDisplay ~ editMode ~ addNodeField
