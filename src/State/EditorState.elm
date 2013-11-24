module State.EditorState where
{- Internal modules -}
import LevelizedGraphs.Graph as Graph
import State.EventRegisters as EventRegisters

type EditorState =
 {selectedNode: Graph.Node
 ,errors: String
 ,misc: String
 ,graph: Graph.Graph
 ,levelizedGraph: [[Graph.Node]]
 ,eventRegister: EventRegisters.RegisteredEvent}

defaultEditorState =
 {selectedNode   = Graph.emptyNode
 ,errors         = ""
 ,misc           = ""
 ,graph          = Graph.sampleGraph
 ,levelizedGraph = Graph.levelizeGraph Graph.sampleGraph
 ,eventRegister  = EventRegisters.NoEvent}

emptyEditorState = 
 {selectedNode   = Graph.emptyNode
 ,errors         = ""
 ,misc           = ""
 ,graph          = []
 ,levelizedGraph = []
 ,eventRegister  = EventRegisters.NoEvent}