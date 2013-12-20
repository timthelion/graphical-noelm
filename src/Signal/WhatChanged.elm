module Signal.WhatChanged where

data EventSource = A | B

setSignal signal value = (\_->value) <~ signal

whatChanged signalA signalB = merge (setSignal signalA A) (setSignal signalB B)