module Main where

import System.Random
import Control.Concurrent  ( threadDelay, forkIO , takeMVar , putMVar , newEmptyMVar , MVar , newMVar , readMVar )

data Customer = Customer {
  name :: Name,
  accountBalance :: AccountBalance,
  accountNumber :: AccountNumber
} deriving (Eq, Show)

data AccountNumber = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten  deriving (Show, Eq)
type AccountBalance =  Int
type Name = String




randomAccountSelectors :: IO (AccountNumber, AccountNumber)
randomAccountSelectors = do
    n <- randomIO :: IO Int
    let random = mapIntToAccount n
    m <- randomIO :: IO Int
    let randomb = mapIntToAccount m
    if random /= randomb then do
    return (random, randomb)
       else do randomAccountSelectors

mapIntToAccount :: Int -> AccountNumber
mapIntToAccount  n = case r of
      0 -> One
      1 -> Two
      2 -> Three
      3 -> Four
      4 -> Five
      5 -> Six
      6 -> Seven
      7 -> Eight
      8 -> Nine
      9 -> Ten 
    where r = mod n 10



main :: IO ()
main = do
 --CREATE 10 VALUES OF TYPE CUSTOMER

 -- Create an account named C1 with a 20 balance
  let c1 = Customer {name = "C1", accountBalance = 20, accountNumber = One}
 -- Create an account named C1 with a 20 balance
  let c2 = Customer {name = "C2", accountBalance = 20, accountNumber = Two} 
 -- Create an account named C3 with a 0 balance
  let c3 = Customer {name = "C3", accountBalance = 20, accountNumber = Three}
-- Create an account named C4 with a 0 balance
  let c4 = Customer {name = "C4", accountBalance = 20, accountNumber = Four}
-- Create an account named C5 with a 0 balance
  let c5 = Customer {name = "C5", accountBalance = 20, accountNumber = Five}
-- Create an account named C6 with a 0 balance
  let c6 = Customer {name = "C6", accountBalance = 20, accountNumber = Six}
-- Create an account named C7 with a 0 balance
  let c7 = Customer {name = "C3", accountBalance = 20, accountNumber = Seven}
-- Create an account named C8 with a 0 balance
  let c8 = Customer {name = "C8", accountBalance = 20, accountNumber = Eight}
-- Create an account named C9 with a 0 balance
  let c9 = Customer {name = "C9", accountBalance = 20, accountNumber = Nine}
-- Create an account named C10 with a 0 balance
  let c10 = Customer {name = "C10", accountBalance = 20, accountNumber = Ten}

-- SPAWN A THREAD FOR EACH CUSTOMER
  a <- newEmptyMVar
  forkIO $ putMVar a c1
  b <- takeMVar a
  c <- newEmptyMVar
  forkIO $ putMVar c c2
  d <- takeMVar c
  print b
  print d
  -- test transfer
  (b, d) <- transfer b d 10
  print b
  print d

-- EACH THREAD SELECT A RANDOM CUSTOMER
-- copy the dice app and each loop (x10)
-- have a customer thread select another customer and transfer money

transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)
transfer from to amount
  | amount <= 0 = return (from, to)
  | accountBalance from < amount = return (from, to)
  | otherwise = return ((from { accountBalance =  ((accountBalance from) - amount)}),(to { accountBalance =  ((accountBalance to) + amount)}))


















{- 07.01 THIS APP runs one thread at a time

data Customer = Customer {
  name :: Name,
  accountBalance :: AccountBalance,
  accountNumber :: AccountNumber
} deriving (Eq, Show)

data AccountNumber = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten  deriving (Show, Eq)
type AccountBalance =  Int
type Name = String
type Winner = String

randomAccountSelector :: IO AccountNumber
randomAccountSelector = do
    n <- randomIO :: IO Int
    let random = mapIntToAccount n
    return random 

mapIntToAccount :: Int -> AccountNumber
mapIntToAccount  n = case r of
      0 -> One
      1 -> Two
      2 -> Three
      3 -> Four
      4 -> Five
      5 -> Six
      6 -> Seven
      7 -> Eight
      8 -> Nine
      9 -> Ten 
    where r = mod n 10
 
main :: IO ()
main = do
    free <- newMVar ()
    random <- randomAccountSelector
    initial_random <- newMVar random
    winner <- newEmptyMVar
    putStrLn "************************************"
    putStrLn "First we pick a random account number to perform the transfer"
    putStrLn $ "Random account that will perform the transfer is: " ++ (show random)
    box <- newEmptyMVar

    forkIO (process "Mia" free box)
    forkIO (process "Tom" free box)
    forkIO (process "Sonia" free box)
    
    forkIO (judge initial_random free box winner)

    w <- takeMVar winner
    putStrLn $ "done"


process :: Name -> MVar () -> MVar (AccountNumber, Name) -> IO () 
process name free box = do
    f <- takeMVar free
    r1 <- randomAccountSelector
    putMVar box (r1, name)
    threadDelay 100
    process name free box

judge :: MVar AccountNumber -> MVar () -> MVar (AccountNumber, Name) -> MVar String -> IO () 
judge initial_random free box winner = do
    r1 <- takeMVar initial_random
    (r2, name) <- takeMVar box
    putStrLn "Next a random customer will be selected - and then they will select a random account number"
    putStrLn "If this account number does not match their own, they will be transferred funds "
    putStrLn "If it does, another customer will pick "
    print $ "Account Name: " ++ name ++ " got " ++ show(r2)
    if r1 /= r2 then do
        putStrLn "************************************"
        putStrLn $ " Account Name: " ++ name ++ " will be transfered funds" 
        putStrLn "************************************"
        putMVar winner name
    else do
        putMVar initial_random r1
        putMVar free ()
        judge initial_random free box winner

-}