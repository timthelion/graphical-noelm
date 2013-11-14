{-
This module contains information about the various edit modes:
 - Keybindings
 - Behavior
 - Documentation

See more licence information at the end of the file, and or in the file COPYING.
-}
module EditModes where
import Keyboard
import Keyboard.Keys

data EditMode = Code | Name | Parents | Delete | Explore | SaveCompile | CodeView | GlobalAdd

data NavigationMode = Move | Insert

type ModeInfo =
 {keyBindings: [Keyboard.Keys.Key]
 ,mode: EditMode
 ,navigationMode: NavigationMode
 ,docs: String}

editModes: [ModeInfo]
editModes =
 [{mode = Explore
  ,navigationMode = Move
  ,keyBindings = [Keyboard.Keys.escape]
  ,docs = "Move arround using the hjkl keys like in Vim."}

 ,{mode = Code
  ,navigationMode = Insert
  ,keyBindings = [Keyboard.Keys.i
                 ,Keyboard.Keys.c]
  ,docs = "Edit the code of a node."}

 ,{mode = Name
  ,navigationMode = Insert
  ,keyBindings = [Keyboard.Keys.n]
  ,docs = "Set the nodes name."}

 ,{mode = Parents
  ,navigationMode = Insert
  ,keyBindings = [Keyboard.Keys.p]
  ,docs = "Set what other nodes this node depends upon."}

 ,{mode = SaveCompile
  ,navigationMode = Move
  ,keyBindings = [Keyboard.Keys.s]
  ,docs = "Save and compile your graphical elm code."}

 ,{mode = CodeView
  ,navigationMode = Move
  ,keyBindings = [Keyboard.Keys.v]
  ,docs = "View the code of each node in the graph view rather than it's name."}

 ,{mode = GlobalAdd
  ,navigationMode = Insert
  ,keyBindings = [Keyboard.Keys.a]
  ,docs = "Add new nodes to your graph or add global attributes like imports and data declarations"}]

getModeInfo: EditMode -> ModeInfo
getModeInfo mode =
 case filter (\em->em.mode == mode) editModes of
  (mi::[]) -> mi

editMode =
 dropRepeats
 <| foldp
  (\pressed oldMode->
   case (getModeInfo oldMode).navigationMode of
    Move -> 
     case filter (\em->any (\kb->kb.keyCode==pressed) em.keyBindings) editModes of
      (em::_) -> em.mode
      _ -> oldMode
    Insert ->
     if | pressed==(.keyCode) Keyboard.Keys.escape -> Explore
        | otherwise -> oldMode)
  Explore
  <| Keyboard.lastPressed

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
