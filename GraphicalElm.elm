module GraphicalElm where
import open List
import Keyboard
import Window
import Graphics.Input
import Either

import open Graph
import open Coordinates
import open CodeGenerator

data EditMode = Code | Language | Name | Parents | Delete

editMode =
 foldp
  (\down oldMode->
   if down
    then
     case oldMode of
      Code -> Language
      Language -> Name
      Name -> Parents
      Parents -> Delete
      Delete -> Code
    else oldMode)
  Code
  <| Keyboard.isDown 123

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

defaultEditorState = {selectedNode=emptyNode,errors="",graph=sampleGraph}

ctrlArrows =
 (\arrs->Arrows arrs)
 <~
  keepWhen
   Keyboard.ctrl
   {x=0,y=0}
   Keyboard.arrows

graphEditorFields = Graphics.Input.fields NoEvent
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
 foldp
  (\fieldEventM ges ->
   case fieldEventM of
    NoEvent       -> ges
    Arrows arrs   -> updateLocation arrs ges
    Replace node  -> {ges|graph <- replaceNode node ges.graph}
    Add node      -> {ges|graph <- addNode node ges.graph}
    DeleteEvent node   -> {ges|graph <- deleteNode node.name ges.graph}
    Rename rename -> {ges|graph <- renameNode rename.oldName rename.newName ges.graph}
    OpenGraph graph -> {defaultEditorState|graph<-graph}
    ParseError err -> {ges|errors<-err}
    )
  defaultEditorState
  <| merges [sampleOn (Keyboard.isDown 13) graphEditorFields.events,graphEditorButtons.events,sampleOn addNodeKeyPress addNodeFields.events,sampleOn loadSavedKeyPress loadSavedFields.events,ctrlArrows]

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
          [ElmLang,FooLang]
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
           newParents=split "," fs.string
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
 in
 if | node.name==selected.name -> selectedElm
    | any (\np->node.name==np) selected.parents -> toText node.name |> Text.color red |> text
    | any (\np->selected.name==np) node.parents ->  toText node.name |> Text.color blue |> text
    | otherwise -> plainText node.name

horizontalLine width =
 collage width 2
  [  traced (solid grey)
  <| segment
      (-(toFloat width/2),0)
      (toFloat width/2,0)]

graphDisplay =
 (\ges em  width height ->
   flow down
    <| intersperse (horizontalLine width)
    <| map (flow right)
    <| map (\level->
        intersperse (toText "|" |> Text.color grey |> text)
        <| map (displayNode ges.selectedNode em) level)
    <| reverse
    <| levelizeGraph ges.graph)
 <~ graphEditorState ~ editMode

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
 (\width height gd em anf ges lsf -> flow down
  [gd width height
  ,horizontalLine width
  ,plainText <| "Edit mode:"++show em++"        Press F12 to change."
  ,flow right [anf,plainText "Press F9 to add node."]
  ,horizontalLine width
  ,plainText "In order to load a saved graph; paste generated code here and press F2 to load."
  ,lsf
  ,toText ges.errors |> Text.color red |> text
  ,horizontalLine width
  ,horizontalLine width
  ,horizontalLine width
  ,plainText <| generateCode ges.graph])
 <~ Window.width ~ Window.height ~ graphDisplay ~ editMode ~ addNodeField ~ graphEditorState ~ loadSavedField
