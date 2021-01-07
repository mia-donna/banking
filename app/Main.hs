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
    putStrLn $ "hi"
