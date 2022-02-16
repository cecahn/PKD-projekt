{-# LANGUAGE BlockArguments #-}
import Control.Exception
import Prelude hiding(catch)
import Data.Binary (Binary(putList))

type Answer = String

{-- randomize
arranges 30 random letters and inserts them in a list
RETURNS: 

--}

randomize = undefined


{-- dictionary t checks if a word exists
RETURNS: 


--}
dictionary :: String -> Bool
dictionary = undefined

{-- letterchecker checks if the letters in the input matches the given ones
RETURNS:

--}
letterchecker :: String -> String -> Bool
letterchecker [] [] = True
letterchecker [] (y:ys) = True
letterchecker (x:xs) [] = False
letterchecker (x:xs) (y:ys)
  | x == y = letterchecker xs ys
  | otherwise = letterchecker (x:xs) ys


{--pointscounter counts the points of the player

--}

pointscounter :: String -> Int -> Int
pointscounter [] acc = acc
pointscounter (x:xs) acc = 1 + (pointscounter xs acc)

{--delete deletes the letters of the input word from the original list

--}
delete:: String -> String -> String
delete _ [] = []
delete [] (y:ys) = y:ys
delete lst (y:ys)
  | head lst == y = delete (tail lst) ys
  | otherwise = y : delete lst ys

listofletters = ['a', 'b', 'c', 'd']
--'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 's', 't', 'u', 'v', 'w', 'y', 'a', 'e', 'i', 'o', 'l', 'c', 'n', 's']



continuePlay :: String -> Int -> IO ()
continuePlay list acc = do
  word <- collectWord
  let newlist = delete word list
  let score = pointscounter word acc
  putStrLn "Here is your total score: "
  print score
  putStrLn "Here is your new list of letters: "
  print newlist
  if null newlist then
      putStrLn "You won, you used all letters from your list!"
  else do
    putStrLn "Do you want to continue? (yes/no): "
    continue <- readAnswer
    if continue == "yes" then continuePlay newlist (pointscounter word acc)
    else
      putStrLn "Thank you for playing!"



main = do
    putStrLn "INSTRUCTIONS"
    putStrLn "Enter your name: "
    gamerName <- getLine
    putStrLn $ "Hey " ++ gamerName ++ " let's play!!"
    putStrLn "Here are your letters: "
    print listofletters
    putStrLn "Good luck!"
    putStrLn "Do you want to continue? (yes/no): "
    continue <- readAnswer
    if continue == "yes" then continuePlay listofletters 0
    else
      putStrLn "Thank you for playing!"


readAnswer :: IO Answer
readAnswer =
  catch (do
  line <- getLine
  evaluate (read line))
  ((\_ -> do
     putStrLn "Invalid input. Correct format: yes/no "
     readAnswer) :: SomeException -> IO Answer)

collectWord :: IO String
collectWord = do
    putStrLn "Enter a word: "
    word <- getLine
    if letterchecker word listofletters then
      return word
    else do
      putStrLn "Invalid input. Use letters from your list."
      collectWord


    