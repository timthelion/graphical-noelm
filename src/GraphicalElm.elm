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

{- Internal modules -}
 {- Gui -}
import Gui.MainGui as MainGui
import Gui.EditModeHelp as EditModeHelp
import Gui.Helpers
import Gui.GraphDisplay as GraphDisplay
import Gui.EditFields as EditFields
import Gui.KeyBindings as KeyBindings
import Gui.LoadSaved as LoadSaved
 {- LevelizedGraphs -}
import LevelizedGraphs.Graph as Graph
 {- ParserAndCompiler -}
import ParserAndCompiler.CodeGenerator as CodeGenerator
 {- State -}
import State.EditorState as EditorState
import State.EditorEvents as EditorEvents
import State.EditorStateMachine as EditorStateMachine
import State.EditModes as EditModes

{- The main program state -}
graphEditorState
 =  EditorStateMachine.graphEditorState
 <| merges
     [editFieldApply
     ,editButtonEvents
     ,loadSavedFieldEvents
     ,movement]

editMode = EditModes.editMode Keyboard.lastPressed

moveMode = EditModes.moveMode <~ editMode

{- Keybindings -}
 {- movement keys -}
ctrlArrows = KeyBindings.ctrlArrows
hjklMovement = KeyBindings.hjklMovement moveMode
movement = merge ctrlArrows hjklMovement

 {- Other key bindings -}

applyKeyPress = KeyBindings.applyKeyPress
loadSavedKeyPress = KeyBindings.loadSavedKeyPress

{- GUI elements -}

graphDisplay
 =  GraphDisplay.graphDisplay
 <~ graphEditorState
 ~  editMode
 ~  Window.width

editFieldS
 =  EditFields.editField
 <~ editMode
 ~  graphEditorState

editFieldApply = EditFields.editFieldApply applyKeyPress

editButtonEvents = EditFields.editButtonEvents

loadSavedField
 =  LoadSaved.loadSavedField
 <~ loadSavedKeyPress

loadSavedFieldEvents = LoadSaved.loadSavedFieldEvents loadSavedKeyPress

gui
 =  MainGui.gui
 <~ Window.width
 ~  graphDisplay
 ~  editMode
 ~  graphEditorState
 ~  loadSavedField
 ~  editFieldS

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
