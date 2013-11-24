{- Graph editor events -}
module State.EditorEvents where
import LevelizedGraphs.Graph as Graph
import State.EventRegisters as EventRegisters
import State.EditorState as EditorState

data EditorEvent
 = NoEvent
 | Replace Graph.Node
 | SetState EditorState.EditorState
 | ParseError String
 | SetMisc String
 | DeleteEvent Graph.Node
 | Arrows {x:Int,y:Int}
 | SetEventRegister EventRegisters.RegisteredEvent
 | ApplyEventRegister