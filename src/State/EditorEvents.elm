{- Graph editor events -}
module State.EditorEvents where
import LevelizedGraphs.Graph as Graph
import State.EditorState as EditorState

data EditorEvent
 = NoEvent
 | Replace Graph.Node
 | SetState EditorState.EditorState
 | ParseError String
 | AddNode Graph.Node
 | SetMisc String
 | DeleteEvent Graph.Node
 | Rename {oldName: String, newName: String}
 | Arrows {x:Int,y:Int}