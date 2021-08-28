module HDL.Hydra.Core.SigStepper where

-- In src/circuits:
-- $ runghc HDL/Hydra/Core/SigStepper

import Data.IORef
import HDL.Hydra.Core.Signal

main :: IO ()
main = do
  putStrLn "hello"
  putStrLn "bye"


data StepperValue a = StepperValue
  { current :: IORef (Maybe a)
  , next :: StepperValue a
  , inputs :: [StepperValue a]
  }

mkStepperValue :: Bit a => a -> [a] => IO (StepperValue a)
mkStepperValue x ys = do
  cv <- newIORef Nothing
  nv <- newIORef Nothing
  let box = StepperValue
              { current = cv
              , next = nv
              , inputs = ys
              }
  return box


constSig :: Sigmal a => IO (StepperValue a)
constSig x = do
  cv <- newIORef Nothing
  nv <- newIORef Nothing
  let box = StepperValue
              { current = cv
              , next = nv
              , inputs = ys
              }
  return box

foo x = do
  q <- newIORef 
