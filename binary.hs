import Control.Monad (forever)
import System.Hardware.Arduino
import Arduino.Util

binary :: IO ()
binary = withArduino False "/dev/ttyACM0" $ do
  let ledVals = [13, 12, 11, 10, 9, 8, 7, 6]
      leds = map digital ledVals
      allOn = map (\led -> digitalWrite led True) leds
      pinModes = map (\x -> setPinMode x OUTPUT) leds
  sequence_ pinModes
  forever $ do
      sequence_ allOn
      delay 100
      allOff ledVals
      delay 100