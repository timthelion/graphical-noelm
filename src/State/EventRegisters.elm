module State.EventRegisters where
import LevelizedGraphs.Graph as Graph
import State.EditModes as EditModes

data RegisteredEvent
 = AddNode Graph.Node
 | Rename {oldName: String, newName: String}
 | NoEvent

isModeWhichRegistersEvents em =
 case em of
  EditModes.AddNode -> True
  EditModes.Name -> True
  _ -> False