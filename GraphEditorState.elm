module GraphEditorState where
import Graph
import Coordinates

type GraphEditorState =
 {selectedNode: Graph.Node
 ,errors: String
 ,misc: [String]
 ,graph: Graph.Graph
 ,levelizedGraph: [[Graph.Node]]
 ,coordinates: [(Coordinates.Coord,Graph.Node)]}

defaultEditorState =
 let
  levelized = Graph.levelizeGraph Graph.sampleGraph
 in
 {selectedNode   = Graph.emptyNode
 ,errors         = ""
 ,misc           = []
 ,graph          = Graph.sampleGraph
 ,levelizedGraph = levelized
 ,coordinates    = Coordinates.coordinates <| levelized}

emptyEditorState = 
 {selectedNode   = Graph.emptyNode
 ,errors         = ""
 ,misc           = []
 ,graph          = []
 ,levelizedGraph = []
 ,coordinates    = []}
