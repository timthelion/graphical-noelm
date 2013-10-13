module Coordinates where
import open List
import open Graph

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
  [] -> {x=0,y=0}

getNode: Coord -> [(Coord,Node)] -> Node
getNode coordToGet coords =
 case coords of
  ((coord,node)::coords') ->
   if | coordToGet==coord -> node
      | otherwise -> getNode coordToGet coords'
  [] -> emptyNode
