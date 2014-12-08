module Arduino.Util
( allOff
) where

import Control.Monad (forever)
import System.Hardware.Arduino

allOff ledVals = 
  do 
    let leds = map digital ledVals
        turnOff = map (\led -> digitalWrite led False) leds
        pinModes = map (\x -> setPinMode x OUTPUT) leds
    sequence_ pinModes
    sequence_ turnOff