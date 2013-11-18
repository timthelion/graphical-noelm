{-

This module provides keybindings for:

 - "Arrow key" bindings for moving around the levelized graph in the graph editor.

 - Form submission, load saved, apply changes to nodes ect

 - It does NOT contain keybindings for entering the various edit modes.  Those can be found in State.EditModes

-}
module Gui.KeyBindings where
{- Standard libraries -}
import Keyboard

{- External libraries -}
import Keyboard.Keys

{- Internal modules -}
import State.EditorEvents

{- "Arrow keys" -}
ctrlArrows =
 (\arrs->State.EditorEvents.Arrows {arrs|y<- -arrs.y}) <~
  keepWhen
   Keyboard.ctrl
   {x=0,y=0}
   Keyboard.arrows

hjklMovement moveMode =
 (\arrs -> State.EditorEvents.Arrows {arrs|y<- -arrs.y}) <~
  keepWhen
   moveMode
   {x=0,y=0}
   (Keyboard.Keys.directionKeys Keyboard.Keys.k Keyboard.Keys.j Keyboard.Keys.h Keyboard.Keys.l)

{- Form submition -}

applyKeyPress
 =  keepWhen (not <~ Keyboard.Keys.isKeyDown Keyboard.Keys.shift) True
 <| keepIf id False
 <| Keyboard.Keys.isKeyDown Keyboard.Keys.enter

loadSavedKeyPress = keepIf id False <| Keyboard.Keys.isKeyDown Keyboard.Keys.f2
