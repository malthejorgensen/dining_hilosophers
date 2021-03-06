import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

main = do
  pHasEaten1 <- newEmptyMVar
  pHasEaten2 <- newEmptyMVar
  pHasEaten3 <- newEmptyMVar
  pHasEaten4 <- newEmptyMVar
  everybodyHasEaten <- newEmptyMVar

  fork1  <- newTVarIO False
  knife1 <- newTVarIO False
  fork2  <- newTVarIO False
  knife2 <- newTVarIO False

  forkIO (philosopher 1 fork1 knife1 pHasEaten1)
  forkIO (philosopher 2 knife1 fork2 pHasEaten2)
  forkIO (philosopher 3 fork2 knife2 pHasEaten3)
  forkIO (philosopher 4 knife2 fork1 pHasEaten4)

  p1 <- takeMVar pHasEaten1
  p2 <- takeMVar pHasEaten2
  p3 <- takeMVar pHasEaten3
  p4 <- takeMVar pHasEaten4

  putStrLn "All done :)"

philosopher i a b c = do
  atomically $ do
    a' <- readTVar a
    b' <- readTVar b
    when (a' || b') retry
    writeTVar a True
    writeTVar b True

  putStrLn ("Philsopher " ++ show i ++ " is eating.")
  threadDelay 1000
  putStrLn ("Philsopher " ++ show i ++ " is done eating.")

  atomically $ do
    writeTVar a False
    writeTVar b False

  putMVar c True
