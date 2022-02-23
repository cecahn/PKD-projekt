import Control.Exception ( SomeException, catch, evaluate )
import Prelude hiding(catch)
import Data.Binary (Binary(putList))
import qualified Table as Ta
import System.IO
import System.Random

type Answer = [Char]


listOfPoints = [('a',1), ('b',2), ('c',2), ('d',1), ('e',1), ('f',4), ('g',2), ('h',3), ('i',1), ('j',4), ('k',3), ('l',1), ('m',3), ('n',1), ('o',2), ('p',3), ('q',10), ('r',1), ('s',1), ('t',1), ('u',3), ('v',4), ('w',2), ('x',10), ('y',8), ('z',10)]
tableOfPoints = Ta.fromList listOfPoints 


main :: IO ()
main = do
    putStrLn "Welcome to Alphapet the computer game"
    putStrLn " "
    putStrLn "INSTRUCTIONS"
    putStrLn "You will be given a list of 30 randomly chosen letters and you need to build as many words as possible using these letters." 
    putStrLn "Each letter is worth a specific number of points so the longer words and the more difficult letters you use will generate a higher score."
    putStrLn "Whenever you don't want to play anymore or cannot build any more words, you can choose to quit the game and you will get all of your words and your final score presented."
    putStrLn "If you manage to use all of your letters you win the game."
    putStrLn " "
    putStrLn "RULES"
    putStrLn "1. Words must be longer than one letter."
    putStrLn "2. Each word must be a valid english word and therefore be included in the english dictionary file (english3.txt)."
    putStrLn "3. You must only use letters from your given list."
    putStrLn " "
    putStrLn "Enter your name: "
    gamerName <- getLine
    putStrLn $ "Hey " ++ gamerName ++ " let's play!!"
    putStrLn " "
    rm <- randomLetters
    putStrLn "Here are your letters: "
    print rm
    putStrLn " "
    putStrLn "Do you want to continue? (yes/no): "
    continue <- readAnswer
    if continue == "yes" then do 
        putStrLn "Good luck!"
        putStrLn " "
        continuePlay rm 0
    else
      putStrLn "Okay, maybe next time!"


{-- randomletters
arranges 30 random letters and inserts them in a list
RETURNS: 

--}
randomLetters :: IO [Char]
randomLetters = fmap  (take 30 . randomRs ('a','z')) newStdGen


-- borrowed from Lab 15
readAnswer :: IO Answer
readAnswer =
  catch (do
  line <- getLine
  evaluate (read line))
  ((\_ -> do
     putStrLn "Invalid input. Correct format: yes/no "
     readAnswer) :: SomeException -> IO Answer)

  
continuePlay :: [Char] -> Int -> IO ()
continuePlay list acc = do
  english <- fmap lines $ readFile "english3.txt"
  word <- collectWord list
  if word == "" then do
    putStrLn ""
  else do
    if word `elem` english then do 
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
        else do
          putStrLn "Thank you for playing!"
          putStrLn "Here is your final score: "
          print score
    else do
      putStrLn "Invalid input. Type a valid english word"
      continuePlay list acc


collectWord :: [Char] -> IO [Char]
collectWord list = do
    putStrLn "Enter a word: "
    word <- getLine
    if length word == 1 then do
      putStrLn "You lose for trying to cheat."
      return ""
    else do
      if letterchecker list word then
        return word
      else do
        putStrLn "Invalid input. Use letters from your list."
        collectWord list


{-- letterchecker checks if the letters in the input matches the given ones
RETURNS:

--}
letterchecker :: Eq a => [a] -> [a] -> Bool
letterchecker [] [] = True
letterchecker (y:ys) [] = True
letterchecker [] (x:xs) = False
letterchecker (y:ys) (x:xs) = letterchecker (lettercheckerAux x (y:ys)) xs

lettercheckerAux :: Eq a => a -> [a] -> [a]
lettercheckerAux _ [] = []
lettercheckerAux x (y:ys) = 
  if x `elem` (y:ys) then
    if x == y then ys
    else y : lettercheckerAux x ys
  else []


{--delete deletes the letters of the input word from the original list

--}
delete :: Eq a => [a] -> [a] -> [a]
delete _ [] = []
delete [] (y:ys) = y:ys
delete lst (y:ys)
  | head lst == y = delete (tail lst) ys
  | otherwise = y : delete lst ys


{--pointscounter counts the points of the player

--}
pointscounter :: [Char] -> Int -> Int
pointscounter [] acc = acc
pointscounter (x:xs) acc = (conv (Ta.lookup tableOfPoints x)) + (pointscounter xs acc)
  where conv (Just x) = x 