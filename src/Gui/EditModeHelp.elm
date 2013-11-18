{-
This module produces help messages for the various edit modes and which hotkeys to use to enter them.

copyright (c) 2013 Timothy Hobbs
Released under the terms of the GNU AGPL v 3 or later.
See more licence information at the end of the file, and or in the file COPYING.
-}
module Gui.EditModeHelp where
{- Standard library imports -}
import List
import Text

{- External library imports -}
import Keyboard.Keys

{- Internal modules -}
import State.EditModes as EditModes


help = flow down
 <| plainText "Edit modes:"
 :: map (\mi->flow right [asText mi.mode,plainText ":",showKeyBindings mi.keyBindings,plainText "|",Text.righted <|Text.toText mi.docs]) EditModes.editModes

showKeyBindings kbs = flow right <| plainText "Hotkey:" :: (map plainText <| List.intersperse " or " <| map (.name) kbs)

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
