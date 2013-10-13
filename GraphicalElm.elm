module GraphicalElm where
import open List
import Keyboard

type Graph = [Node]
type Node = {parents:[String],name:String,value:String}

data Ord = Gt | Lt | Eq

naiveSortBy: (a -> a -> Ord) -> [a] -> [a]
naiveSortBy comp al = naiveSortBy' comp al [] []
naiveSortBy' comp source bucketLess bucketMore =
 case source of
  (a1::a2::ass) -> case comp a1 a2 of
                   Gt -> naiveSortBy' comp (a2::ass) (bucketLess) (a1::bucketMore)
                   Lt -> naiveSortBy' comp (a2::ass) (bucketLess++[a1]) bucketMore
                   Eq -> naiveSortBy' comp (a2::ass) (bucketLess++[a1]) bucketMore
  (a::[]) -> case (bucketLess,bucketMore) of
              ([],[]) -> [a]
              ((bl::bls),[]) -> case comp a (last bucketLess) of
                                Gt -> bucketLess++[a]
                                Eq -> bucketLess++[a]
                                Lt -> naiveSortBy' comp (a::bucketLess) [] []
              (_,_) -> naiveSortBy' comp (bucketLess++[a]++bucketMore) [] []
  [] -> case bucketMore of
         [] -> bucketLess
         _ -> naiveSortBy' comp (bucketLess++bucketMore) [] []

--VERY NAIVE IMPLEMENTATION, very inefficient, goes into infinite loop in case of graph cyclicity.
sortGraph: Graph -> Graph
sortGraph g = reverse <| sortGraph' [] <| naiveSortBy (\n1 n2-> if | length n1.parents > length n2.parents -> Gt
                                                                   | length n1.parents == length n2.parents -> Eq
                                                                   | otherwise -> Lt) g

sortGraph' alreadySorted ns =
 case ns of
  (node::nodes) ->
    let inAlreadySorted n = any (\asn->asn.name==n) alreadySorted
    in
    if | any inAlreadySorted node.parents || node.parents==[] -> sortGraph' (node::alreadySorted) nodes
       | otherwise -> sortGraph' alreadySorted <| nodes ++(node::[])
  [] -> alreadySorted

levelizeGraph: Graph -> [[Node]]
levelizeGraph g = levelizeGraph' [] <| sortGraph g
levelizeGraph': [[Node]] -> Graph -> [[Node]]
levelizeGraph' ls ns =
 case (ls,ns) of
   ((thisLevel::higherLevels),(node::nodes))->
    let
     hasParentInThisLevel = any (\pp-> any (\p->p==pp.name) node.parents) thisLevel
    in
    if | hasParentInThisLevel -> levelizeGraph' ([node]::thisLevel::higherLevels) nodes
       | otherwise -> levelizeGraph' ((node::thisLevel)::higherLevels) nodes
   ([],(node::nodes)) ->  levelizeGraph' [[node]] nodes
   (levels,[]) -> levels

sampleGraph =
 [{parents=[],name="arrows",value=""}
 ,{parents=["arrows"],name="loc",value=""}
 ,{parents=[],name="mouse",value=""}
 ,{parents=["loc","mouse"],name="main",value=""}
 ]

type GraphEditorState =
 {selectedNode: String
 ,graph: Graph}

type Coord = {x:Int,y:Int}

coordinates: Graph -> [(Coord,Node)]
coordinates graph = levelizeGraph graph |> reverse |> coordinates' 0 |> concat

coordinates': Int -> [[Node]] -> [[(Coord,Node)]]
coordinates' n nodes' =
 case nodes' of
  (nodes::nodess) -> (coordinates'' nodes 0 n)::coordinates' (n+1) nodess
  [] -> []
coordinates'': [Node] -> Int -> Int -> [(Coord,Node)]
coordinates'' nodes' x y =
 case nodes' of
  (node::nodes) -> ({x=x,y=y},node)::coordinates'' nodes (x+1) y
  [] -> []

getCoord: String -> [(Coord,Node)] -> Coord
getCoord name coords =
 case coords of
  ((coord,node)::coords') ->
   if | name==node.name -> coord
      | otherwise -> getCoord name coords'

getNode: Coord -> [(Coord,Node)] -> Node
getNode coordToGet coords =
 case coords of
  ((coord,node)::coords') ->
   if | coordToGet==coord -> node
      | otherwise -> getNode coordToGet coords'
  [] -> {parents=[],name="",value=""}

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
