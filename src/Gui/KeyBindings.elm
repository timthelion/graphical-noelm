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
import State.EditorEvents as EditorEvents
import State.EditModes as EditModes
import State.EventRegisters as EventRegisters

{- "Arrow keys" -}
ctrlArrows
 = (\arrs->EditorEvents.Arrows {arrs|y<- -arrs.y}) <~ (onlyMovement
 (keepWhen
   Keyboard.ctrl
   {x=0,y=0}
   Keyboard.arrows))

hjklMovement moveMode
 = (\arrs -> EditorEvents.Arrows {arrs|y<- -arrs.y}) <~ onlyMovement
 (keepWhen
   moveMode
   {x=0,y=0}
   (Keyboard.Keys.directionKeys Keyboard.Keys.k Keyboard.Keys.j Keyboard.Keys.h Keyboard.Keys.l))

onlyMovement: Signal {x:number,y:number} -> Signal {x:number,y:number}
onlyMovement dirs = keepIf (\dirs-> dirs.x /= 0 || dirs.y /= 0) {x=0,y=0} dirs

{- Form submition -}

applyKeyPress
 em
 = (\_->EditorEvents.ApplyEventRegister)
 <~ (keepWhen ((\em->EventRegisters.isModeWhichRegistersEvents em) <~ em) False
 <| keepIf id False
 <| merge
     (Keyboard.Keys.isKeyDown Keyboard.Keys.enter)
     (Keyboard.Keys.isKeyDown Keyboard.Keys.escape))