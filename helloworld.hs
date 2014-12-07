import Control.Monad       (forever)
import System.Hardware.Arduino
import Data.Char

-- | Add two binary numbers together. Assume that the first number is at least
-- as large as the second. Char's are zeros and ones.
binaryAdd :: [Int] -> [Int] -> [Int]
binaryAdd xs ys = binaryAddHelper (reverse xs) (reverse ys) 0 []

-- digits = map digitToInt

binaryAddHelper [] _ carryBit result
  | carryBit == 0 = result
  | carryBit == 1 = 1 : result
binaryAddHelper (x : xs) ys carryBit result = binaryAddHelper xs ys' carryBit' result'
  where
    result' = ((x + y + carryBit) `mod` 2) : result
    carryBit' = div (x + y + carryBit) 2
    y = if length ys == 0
          then 0
          else head ys
    ys' = if length ys == 0
            then ys
            else tail ys

binaryAddHelper' [] _ carryBit result
  | carryBit == 0 = ([], [], 0, result)
  | carryBit == 1 = ([], [], 0, (1 : result))
binaryAddHelper' (x : xs) ys carryBit result = (reverse xs, reverse ys', carryBit', result')
  where
    result' = ((x + y + carryBit) `mod` 2) : result
    carryBit' = div (x + y + carryBit) 2
    y = if length ys == 0
          then 0
          else head ys
    ys' = if length ys == 0
            then ys
            else tail ys

allOff :: Arduino ()
allOff = do 
            let ledVals = [13, 12, 11, 10, 9]
                leds = map digital ledVals
                turnOff = map (\led -> digitalWrite led False) leds
            sequence_ turnOff

helloworld :: IO ()
helloworld = withArduino False "/dev/ttyACM0" $ do
              let carryLed = digital 9
                  xs = [1, 1, 1]
                  ys = [1, 1]
                  xs' = take (4 - length xs) (repeat 0) ++ xs
                  ys' = take (4 - length ys) (repeat 0) ++ ys
                  result = binaryAdd xs ys
                  result' = take (4 - length result) (repeat 0) ++ result

                  ledVals = [13, 12, 11, 10]
                  leds = map digital ledVals
                  pairs = zip leds result'

                  firstDisplay = map (\(a, b) -> if b == 1 then (digitalWrite a True) else (digitalWrite a False)) (zip leds xs')
                  secondDisplay = map (\(a, b) -> if b == 1 then (digitalWrite a True) else (digitalWrite a False)) (zip leds ys')
                  resultDisplay = map (\(a, b) -> if b == 1 then (digitalWrite a True) else (digitalWrite a False)) pairs
                  pinModes = map (\x -> setPinMode x OUTPUT) leds
              setPinMode carryLed OUTPUT
              sequence_ pinModes
              forever $ do
                allOff
                sequence_ firstDisplay
                delay 250
                allOff
                digitalWrite carryLed True
                delay 250
                digitalWrite carryLed False
                sequence_ secondDisplay
                delay 250
                allOff
                digitalWrite carryLed True
                delay 250
                digitalWrite carryLed False
                sequence_ resultDisplay
                delay 250
                allOff
                delay 250
