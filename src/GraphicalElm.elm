{-GENERATED BY GRAPHICALELM 1.0-}
{-START MISC-}
{-
This module is the main module for the program.

It handles drawing of all GUI components.

It hands out the task of sorting and levelizing graphs to the Graph module.
The main types are also found in the Graph module.

The Coordinates module is a simple helper module to find the indexes of an item in a 2D list.

The CodeGenerator module handles BOTH parsing and code generation.

copyright (c) 2013 Timothy Hobbs
Released under the terms of the GNU AGPL v 3 or later.

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

{-END MISC-}
main = mainPrivateDon'tUse
lastKeyPressPrivateDon'tUse =Keyboard.lastPressed {-_language_LiftElm_base_code_#####$#Keyboard.lastPressed#$#####_parents_-}
windowWidthPrivateDon'tUse =Window.width {-_language_LiftElm_base_code_#####$#Window.width#$#####_parents_-}
graphEditorStatePrivateDon'tUse =(\editorEvents->EditorStateMachine.graphEditorState  editorEvents)editorEventsPrivateDon'tUse{-_language_LiftElm_base_code_#####$#EditorStateMachine.graphEditorState #$#####_parents_editorEvents-}
editorEventsPrivateDon'tUse =(\editFieldEvents editButtonEvents movement applyKeyPress->merges [editFieldEvents,editButtonEvents,movement,applyKeyPress])editFieldEventsPrivateDon'tUse editButtonEventsPrivateDon'tUse movementPrivateDon'tUse applyKeyPressPrivateDon'tUse{-_language_ElmLang_base_code_#####$#merges [editFieldEvents,editButtonEvents,movement,applyKeyPress]#$#####_parents_editFieldEvents,editButtonEvents,movement,applyKeyPress-}
editModePrivateDon'tUse =(\lastKeyPress->EditModes.editMode  lastKeyPress)lastKeyPressPrivateDon'tUse{-_language_LiftElm_base_code_#####$#EditModes.editMode #$#####_parents_lastKeyPress-}
moveModePrivateDon'tUse =(\editMode->EditModes.moveMode <~ editMode)editModePrivateDon'tUse{-_language_LiftElm_base_code_#####$#EditModes.moveMode <~#$#####_parents_editMode-}
movementPrivateDon'tUse =(\ctrlArrows hjklMovement->merge ctrlArrows hjklMovement)ctrlArrowsPrivateDon'tUse hjklMovementPrivateDon'tUse{-_language_ElmLang_base_code_#####$#merge ctrlArrows hjklMovement#$#####_parents_ctrlArrows,hjklMovement-}
hjklMovementPrivateDon'tUse =(\moveMode->KeyBindings.hjklMovement  moveMode)moveModePrivateDon'tUse{-_language_LiftElm_base_code_#####$#KeyBindings.hjklMovement #$#####_parents_moveMode-}
ctrlArrowsPrivateDon'tUse =KeyBindings.ctrlArrows {-_language_LiftElm_base_code_#####$#KeyBindings.ctrlArrows#$#####_parents_-}
applyKeyPressPrivateDon'tUse =(\editMode->KeyBindings.applyKeyPress  editMode)editModePrivateDon'tUse{-_language_LiftElm_base_code_#####$#KeyBindings.applyKeyPress #$#####_parents_editMode-}
redrawPrivateDon'tUse =(\editMode movement applyKeyPress editButtonEvents->merges [(\ _ -> True) <~ editMode,(\ _ -> True)<~ applyKeyPress,(\ _ -> True) <~ movement,(\ _ -> True)<~editButtonEvents])editModePrivateDon'tUse movementPrivateDon'tUse applyKeyPressPrivateDon'tUse editButtonEventsPrivateDon'tUse{-_language_ElmLang_base_code_#####$#merges [(\ _ -> True) <~ editMode,(\ _ -> True)<~ applyKeyPress,(\ _ -> True) <~ movement,(\ _ -> True)<~editButtonEvents]#$#####_parents_editMode,movement,applyKeyPress,editButtonEvents-}
graphDisplayPrivateDon'tUse =(\redraw graphEditorState editMode windowWidth->GraphDisplay.graphDisplay <~ (sampleOn redraw graphEditorState) ~ editMode ~ windowWidth)redrawPrivateDon'tUse graphEditorStatePrivateDon'tUse editModePrivateDon'tUse windowWidthPrivateDon'tUse{-_language_ElmLang_base_code_#####$#GraphDisplay.graphDisplay <~ (sampleOn redraw graphEditorState) ~ editMode ~ windowWidth#$#####_parents_redraw,graphEditorState,editMode,windowWidth-}
editFieldBuilderSPrivateDon'tUse =(\redraw editMode graphEditorState->sampleOn redraw <| EditFields.editFieldBuilder <~ editMode ~ graphEditorState)redrawPrivateDon'tUse editModePrivateDon'tUse graphEditorStatePrivateDon'tUse{-_language_ElmLang_base_code_#####$#sampleOn redraw <| EditFields.editFieldBuilder <~ editMode ~ graphEditorState#$#####_parents_redraw,editMode,graphEditorState-}
editFieldSPrivateDon'tUse =(\editFieldBuilderS editFieldStates->EditFields.editField <~ editFieldBuilderS ~ editFieldStates ~ WhatChanged.whatChanged editFieldBuilderS editFieldStates)editFieldBuilderSPrivateDon'tUse editFieldStatesPrivateDon'tUse{-_language_ElmLang_base_code_#####$#EditFields.editField <~ editFieldBuilderS ~ editFieldStates ~ WhatChanged.whatChanged editFieldBuilderS editFieldStates#$#####_parents_editFieldBuilderS,editFieldStates-}
editFieldStatesPrivateDon'tUse =(\editFieldStateEvents editFieldMultilineStateEvents->EditFields.fieldStates <~ merge editFieldStateEvents editFieldMultilineStateEvents)editFieldStateEventsPrivateDon'tUse editFieldMultilineStateEventsPrivateDon'tUse{-_language_ElmLang_base_code_#####$#EditFields.fieldStates <~ merge editFieldStateEvents editFieldMultilineStateEvents#$#####_parents_editFieldStateEvents,editFieldMultilineStateEvents-}
editFieldEventsPrivateDon'tUse =(\editFieldStateEvents editFieldMultilineStateEvents->EditFields.editorFieldEvents <~ merge editFieldStateEvents editFieldMultilineStateEvents)editFieldStateEventsPrivateDon'tUse editFieldMultilineStateEventsPrivateDon'tUse{-_language_ElmLang_base_code_#####$#EditFields.editorFieldEvents <~ merge editFieldStateEvents editFieldMultilineStateEvents#$#####_parents_editFieldStateEvents,editFieldMultilineStateEvents-}
editFieldStateEventsPrivateDon'tUse =(.events) EditFields.editorFields {-_language_LiftElm_base_code_#####$#(.events) EditFields.editorFields#$#####_parents_-}
editFieldMultilineStateEventsPrivateDon'tUse =(.events) EditFields.editorFieldsMultiline {-_language_LiftElm_base_code_#####$#(.events) EditFields.editorFieldsMultiline#$#####_parents_-}
editButtonEventsPrivateDon'tUse =EditFields.editButtonEvents {-_language_LiftElm_base_code_#####$#EditFields.editButtonEvents#$#####_parents_-}
guiPrivateDon'tUse =(\windowWidth graphDisplay editMode graphEditorState editFieldS editorEvents->MainGui.gui <~ windowWidth~graphDisplay~editMode~graphEditorState~editFieldS~editorEvents)windowWidthPrivateDon'tUse graphDisplayPrivateDon'tUse editModePrivateDon'tUse graphEditorStatePrivateDon'tUse editFieldSPrivateDon'tUse editorEventsPrivateDon'tUse{-_language_LiftElm_base_code_#####$#MainGui.gui <~#$#####_parents_windowWidth,graphDisplay,editMode,graphEditorState,editFieldS,editorEvents-}
mainPrivateDon'tUse =(\gui->gui)guiPrivateDon'tUse{-_language_ElmLang_base_code_#####$#gui#$#####_parents_gui-}
