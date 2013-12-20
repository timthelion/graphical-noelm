{-
This module is the main module for the program.

It handles drawing of all GUI components.

It hands out the task of sorting and levelizing graphs to the Graph module.
The main types are also found in the Graph module.

The Coordinates module is a simple helper module to find the indexes of an item in a 2D list.

The CodeGenerator module handles BOTH parsing and code generation.

copyright (c) 2013 Timothy Hobbs
Released under the terms of the GNU AGPL v 3 or later.
See more licence information at the end of the file, and or in the file COPYING.
-}
module GraphicalElm where
{- Base library imports -}
import Window
import Keyboard

{- External libraries -}
import Signal.WhatChanged as WhatChanged

{- Internal modules -}
 {- Gui -}
import Gui.MainGui as MainGui
import Gui.EditModeHelp as EditModeHelp
import Gui.Helpers
import Gui.GraphDisplay as GraphDisplay
import Gui.EditFields as EditFields
import Gui.KeyBindings as KeyBindings
 {- LevelizedGraphs -}
import LevelizedGraphs.Graph as Graph
 {- State -}
import State.EditorState as EditorState
import State.EditorEvents as EditorEvents
import State.EditorStateMachine as EditorStateMachine
import State.EditModes as EditModes

{- The main program state -}
graphEditorState
 =  EditorStateMachine.graphEditorState editorEvents

editorEvents = merges
     [editFieldEvents
     ,editButtonEvents
     ,movement
     ,applyKeyPress]

editMode = EditModes.editMode Keyboard.lastPressed

moveMode = EditModes.moveMode <~ editMode

{- Keybindings -}
 {- movement keys -}
ctrlArrows = KeyBindings.ctrlArrows
hjklMovement = KeyBindings.hjklMovement moveMode
movement = merge ctrlArrows hjklMovement

 {- Other key bindings -}

applyKeyPress = KeyBindings.applyKeyPress editMode

{- GUI elements -}

redraw = merges [((\_->True)<~ editMode),((\_->True)<~movement),((\_->True)<~applyKeyPress)]

graphDisplay
 =  GraphDisplay.graphDisplay
 <~ (sampleOn redraw graphEditorState)
 ~  editMode
 ~  Window.width

editFieldBuilderS
 =  sampleOn redraw
 <| EditFields.editFieldBuilder
 <~ editMode
 ~  graphEditorState

editFieldS
 =  EditFields.editField
 <~ editFieldBuilderS
 ~  editFieldStates
 ~  WhatChanged.whatChanged editFieldBuilderS editFieldStates

editFieldStates = EditFields.fieldStates <~ merge editFieldStateEvents editFieldMultilineStateEvents

editFieldEvents = EditFields.editorFieldEvents <~ merge editFieldStateEvents editFieldMultilineStateEvents

editFieldStateEvents = (.events) EditFields.editorFields

editFieldMultilineStateEvents = (.events) EditFields.editorFieldsMultiline

editButtonEvents = EditFields.editButtonEvents

gui
 =  MainGui.gui
 <~ Window.width
 ~  graphDisplay
 ~  editMode
 ~  graphEditorState
 ~  editFieldS
 ~  editorEvents

main = gui

{-
Graphical ELM - A program for editing graphs as graphs.
Visually, Architecturally

    Copyright (C) 2013  Timothy Hobbs <timothyhobbs@seznam.cz> thobbs.cz

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
