{- This module provides the graphDisplay function which takes a graph and displays it as a properly collorized levelized graph.
-}
module Gui.GraphDisplay where
{- External library imports -}
import String

{- Internal modules -}
import State.EditModes as EditModes
import LevelizedGraphs.Graph as Graph
import Gui.Helpers

displayNode: EditModes.EditMode -> Graph.Node -> Graph.Node -> Element
displayNode mode selected node =
 let nodeString = case mode of
       EditModes.CodeView -> node.value.code
       EditModes.TypeView ->
        case node.value.ntype of
         Just ntype -> ntype
         Nothing -> ""
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

graphDisplay ges em width =
   flow down
    <| intersperse (Gui.Helpers.horizontalLine width)
    <| map (flow right)
    <| map (\level->
        intersperse Gui.Helpers.verticalLine
        <| map (displayNode em ges.selectedNode) level)
    <| ges.levelizedGraph