
module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  -- me :: Int is the port number of this process
  -- neighbours :: [Int] is a list of the port numbers of the initial neighbours
  -- During the execution, connections may be broken or constructed
  (me, neighbours) <- readCommandLineArguments

  putStrLn $ "I should be listening on port " ++ show me
  putStrLn $ "My initial neighbours are " ++ show neighbours

  -- Listen to the specified port.
  serverSocket <- socket AF_INET Stream 0
  setSocketOption serverSocket ReuseAddr 1
  bind serverSocket $ portToAddress me
  listen serverSocket 1024
  -- Let a seperate thread listen for incomming connections
  _ <- forkIO $ listenForConnections serverSocket


  putStrLn $ show (updateRT [1,0,0,2,1,2,3,2,2,4,3,3] [4,1,4,5,1,5] [1,0,0,2,1,2,3,2,2,4,3,3] 2)


  -- Prints the first RT 
  -- _ <- printRT (generateOwnRT me (me:neighbours))

  -- As an example, connect to the first neighbour. This just
  -- serves as an example on using the network functions in Haskell
  case neighbours of
    [] -> putStrLn "I have no neighbours :("
    neighbour : _ -> do
      putStrLn $ "Connecting to neighbour " ++ show neighbour ++ "..."
      client <- connectSocket neighbour
      chandle <- socketToHandle client ReadWriteMode
      -- Send a message over the socket
      -- You can send and receive messages with a similar API as reading and writing to the console.
      -- Use `hPutStrLn chandle` instead of `putStrLn`,
      -- and `hGetLine  chandle` instead of `getLine`.
      -- You can close a connection with `hClose chandle`.
      



      --hPutStrLn chandle $ "Hi process " ++ show neighbour ++ "! I'm process " ++ show me ++ " and you are my first neighbour."
      hPutStrLn chandle $ show (generateOwnRT me (me:neighbours))

      putStrLn "I sent a message to the neighbour"
      message <- hGetLine chandle
      putStrLn $ "Neighbour send a message back: " ++ show message
      hClose chandle

  threadDelay 1000000000

readCommandLineArguments :: IO (Int, [Int])
readCommandLineArguments = do
  args <- getArgs
  case args of
    [] -> error "Not enough arguments. You should pass the port number of the current process and a list of neighbours"
    (me:neighbours) -> return (read me, map read neighbours)

portToAddress :: Int -> SockAddr
portToAddress portNumber = SockAddrInet (fromIntegral portNumber) (tupleToHostAddress (127, 0, 0, 1)) -- localhost

connectSocket :: Int -> IO Socket
connectSocket portNumber = connect'
  where
    connect' = do
      client <- socket AF_INET Stream 0
      result <- try $ connect client $ portToAddress portNumber
      case result :: Either IOException () of
        Left _ -> do
          threadDelay 1000000
          connect'
        Right _ -> return client

listenForConnections :: Socket -> IO ()
listenForConnections serverSocket = do
  (connection, _) <- accept serverSocket
  _ <- forkIO $ handleConnection connection
  listenForConnections serverSocket

handleConnection :: Socket -> IO ()
handleConnection connection = do
  putStrLn "Got new incomming connection"
  chandle <- socketToHandle connection ReadWriteMode
  hPutStrLn chandle "Welcome"
  message <- hGetLine chandle
  putStrLn $ "Incomming connection send a message: " ++ message
  hClose chandle


generateOwnRT :: Int -> [Int] -> [Int]
generateOwnRT _ []           = []
generateOwnRT ownPort (p:ps) | p == ownPort = p : 0 : 0 : generateOwnRT ownPort (ps)
                             | otherwise = p : 1 : p : generateOwnRT ownPort (ps)


updateRT :: [Int] -> [Int] -> [Int] -> Int -> [Int]
updateRT _                          []                                 original _      = 
  original

updateRT []                         (receivedD:receivedS:receivedV:ys) original sender =
  updateRT (original ++ receivedD:(receivedS + 1):sender:[]) ys (original ++ receivedD:(receivedS + 1):sender:[]) sender

updateRT (destination:steps:via:xs) (receivedD:receivedS:receivedV:ys) original sender =
  if destination == receivedD
    then if steps > (receivedS + 1)
      then updateRT newOriginal ys newOriginal sender
      else updateRT original ys original sender
    else updateRT xs (receivedD:receivedS:receivedV:ys) original sender
      where newOriginal = updateThisRecord receivedD (receivedS + 1) sender original
    
-- stable updater:
updateThisRecord destination uSteps uVia (z1:z2:z3:zs) =
  if destination == z1
    then destination : uSteps : uVia : zs
    else z1 : z2 : z3 : updateThisRecord destination uSteps uVia zs


printRT :: [Int] -> IO()
printRT (destination:steps:via:xs) = do
  if via == 0
    then putStrLn $ show destination ++ " " ++ show steps ++ " local"
    else putStrLn $ show destination ++ " " ++ show steps ++ " " ++ show via
  printRT (xs)
  return ()