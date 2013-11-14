{- Graph editor events -}
module GraphEditorEvents where
import Graph
import GraphEditorState

data GraphEditorEvent
 = NoEvent
 | Replace Graph.Node
 | SetState GraphEditorState.GraphEditorState
 | ParseError String
 | AddNode Graph.Node
 | AddMisc String
 | DeleteEvent Graph.Node
 | Rename {oldName: String, newName: String}
 | Arrows {x:Int,y:Int}