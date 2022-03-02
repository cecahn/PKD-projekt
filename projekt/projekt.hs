-- ..
import Control.Exception ( SomeException, catch, evaluate )
import Prelude hiding(catch)
import Data.Binary (Binary(putList))
import qualified Table as Ta
import System.IO
import System.Random


type Answer = [Char]


listOfPoints = [('a',1), ('b',2), ('c',2), ('d',1), ('e',1), ('f',4), ('g',2), ('h',3), ('i',1), ('j',4), ('k',3), ('l',1), ('m',3), ('n',1), ('o',2), ('p',3), ('q',10), ('r',1), ('s',1), ('t',1), ('u',3), ('v',4), ('w',2), ('x',10), ('y',8), ('z',10)]
tableOfPoints = Ta.fromList listOfPoints

{-- main
provides the game instruction for player and prints out the list of the letters which are avalible to use
PRE: True
RETURNS: Fixed strings containing the instructions for the game and various different strings
depending on the players input.
EXAMPLE:  
Welcome to Alphapet the computer game
 
INSTRUCTIONS
You will be given a list of 30 randomly chosen letters and you need to build as many words as possible using these letters.
Each letter is worth a specific number of points so the longer words and the more difficult letters you use will generate a higher score.
Whenever you don't want to play anymore or cannot build any more words, you can choose to quit the game and you will get all of your words and your final score presented.
If you manage to use all of your letters you win the game.
 
RULES
1. Inputs must be longer than one letter.
2. Each input must be a valid english word and therefore be included in the english dictionary file (english3.txt).
3. You must only use letters from your given list.
4. Each new input must begin with the last letter of the previous word.
 
Enter your name: 
Cissi
Hey Cissi let's play!!
 
Here are your letters: 
"trzhnsbjrocmqkuriuicakuszqmoqu"
 
Do you want to continue? "yes"/"no": 
"yes"
Good luck!

--}

main :: IO ()
main = do
    putStrLn " "
    putStrLn "Welcome to Alphapet the computer game"
    putStrLn " "
    putStrLn "INSTRUCTIONS"
    putStrLn "You will be given a list of 30 randomly chosen letters and you need to build as many words as possible using these letters."
    putStrLn "Each letter is worth a specific number of points so the longer words and the more difficult letters you use will generate a higher score."
    putStrLn "Whenever you don't want to play anymore or cannot build any more words, you can choose to quit the game and you will get all of your words and your final score presented."
    putStrLn "If you manage to use all of your letters you win the game."
    putStrLn " "
    putStrLn "RULES"
    putStrLn "1. Inputs must be longer than one letter."
    putStrLn "2. Each input must be a valid english word and therefore be included in the english dictionary file (english3.txt)."
    putStrLn "3. You must only use letters from your given list."
    putStrLn "4. Each new input must begin with the last letter of the previous word."
    putStrLn " "
    putStrLn "Enter your name: "
    gamerName <- getLine
    putStrLn $ "Hey " ++ gamerName ++ " let's play!!"
    putStrLn " "
    rm <- randomLetters
    putStrLn "Here are your letters: "
    print rm
    putStrLn " "
    putStrLn "Do you want to continue? \"yes\"/\"no\": "
    continue <- readAnswer
    if continue == "yes" then do
        putStrLn "Good luck!"
        putStrLn " "
        continuePlay rm 0 '.' []
    else
      putStrLn "Okay, maybe next time!"


{-- randomletters
Arranges 30 random letters and inserts them in a list
PRE: TRUE
RETURNS: a string with 30 letters
EXAMPLES: 
  randomLetters == "rwpnxhbyqzhjdasqsrgwentzktldyy"
  randomLetters == "hmsmewwiqjepqtgasvzciwucolkdqh"
--}

randomLetters :: IO [Char]
randomLetters = fmap  (take 30 . randomRs ('a','z')) newStdGen

{- readAnswer 
evaluates a players input to see if it is the desired one
PRE: TRUE  
RETURNS: If the answer is not our desired one it prints a string asking the player to try again.
Otherwise it returns the answer 
EXAMPLE: readAnswer
"yes"
"yes" 
readAnswer
"no"
"no"
readAnswer
hej
Invalid input. Correct format: yes/no

-}

readAnswer :: IO Answer
readAnswer =
  catch (do
  line <- getLine
  evaluate (read line))
  ((\_ -> do
     putStrLn "Invalid input. Correct format: yes/no "
     putStrLn " "
     readAnswer) :: SomeException -> IO Answer)

{- continuePlay list acc char wordlist
reads the file english3 and checks the approved words against it. If the word is valid the function
stores the word, the last char, the players score and deletes the letters in the word from the players
list. 
PRE:
RETURNS: The list of the words that's been used throughout the run, the players total score, new 
list of letters and the letter the next word has to begin with. Then it either restarts or thanks 
the player for playing depending on the players input. 
EXAMPLE:Here are all your words for this round: 
["far"]
 
Here is your total score: 
6
 
Here is your new list of letters: 
"lqfhzkvecsoryctmsqseljnadbnn"
 
Here is the letter your next word has to begin with: 
'r'
 
Do you want to continue? ("yes"/"no") : 
"yes"
 
Enter a word:  
aey
Invalid input. Type a valid english word
 
 
Enter a word: 
am
 
Here are all your words for this round: 
["am","ta","fit"]
 
Here is your total score: 
12
 
Here is your new list of letters: 
"ismtivupyuzujaoeeypjwmlgse"
 
Here is the letter your next word has to begin with: 
'm'
 
Do you want to continue? ("yes"/"no") : 
"no"
 
Thank you for playing!
 
Here is your final score: 
 
2
-}

continuePlay :: [Char] -> Int -> Char -> [[Char]] -> IO ()
continuePlay list acc char wordlist = do
  english <- fmap lines $ readFile "english3.txt"
  word <- collectWord list char
  if word == "" then do
    putStrLn ""
  else do
    if word `elem` english then do
      let nextfirst = last word
      let newlist = delete (init word) list
      let score = pointscounter word acc
      putStrLn " "
      putStrLn "Here are all your words for this round: "
      print (word : wordlist)
      putStrLn " "
      putStrLn "Here is your total score: "
      print score
      putStrLn " "
      putStrLn "Here is your new list of letters: "
      print newlist
      putStrLn " "
      putStrLn "Here is the letter your next word has to begin with: "
      print nextfirst
      putStrLn " "
      if length newlist == 1 then do
        putStrLn "You won, you used all letters from your list!"
      else do
        putStrLn "Do you want to continue? (\"yes\"/\"no\") : "
        continue <- readAnswer
        if continue == "yes" then continuePlay newlist (pointscounter word acc) nextfirst (word : wordlist)
        else do
          putStrLn " "
          putStrLn "Thank you for playing!"
          putStrLn " "
          putStrLn "Here is your final score: "
          putStrLn " "
          print score
    else do
      putStrLn "Invalid input. Type a valid english word"
      putStrLn " "
      continuePlay list acc char wordlist

{- collectWord list char
Takes an input from the user and checks if the word contains more than one letter and only contains letters
  which the game has provided
SIDE EFFECTS: reads in users input and prints out the game interface
RETURN:  
EXAMPLES: 
  *Main> collectWord "abcdan" '.'
  Enter a word: 
  can
  "can"

  *Main> collectWord "abcdan" '.'
  Enter a word: 
  hey
  Invalid input. Use letters from your list.
 
  Enter a word: 

  *Main> collectWord "abcd" '.'
  Enter a word: 
  c
  "You lose for trying to cheat." 


  *Main> collectWord "hcyaun" 'j'
  Enter a word: 
  can
  You must use the last character from your previous word as the first letter of your current word.
  That letter is: 
  'j'
 
  Enter a word: 


-}

collectWord :: [Char] -> Char -> IO [Char]
collectWord list char = do
    putStrLn " "
    putStrLn "Enter a word: "
    word <- getLine
    if (length word) == 0 then do
      putStrLn "You must enter a word as input."
      putStrLn " "
      collectWord list char
    else do
      if (length word) == 1 then do
        putStrLn "You lose for trying to cheat."
        return ""
      else do
        if char == '.' then do
          if letterchecker list word then
            return word
          else do
            putStrLn "Invalid input. Use letters from your list."
            putStrLn " "
            collectWord list char
        else
          if head word == char then do
            if letterchecker list (tail word) then
              return word
            else do
              putStrLn "Invalid input. Use letters from your list."
              putStrLn " "
              collectWord list char
          else do
            putStrLn "You must use the last character from your previous word as the first letter of your current word."
            putStrLn "That letter is: "
            print char
            putStrLn " "
            collectWord list char


{-- letterchecker (y:ys) (x:xs)
checks if the letters in (x:xs) are included in (y:ys)
PRE: True
RETURNS: True if the letters (x:xs) are included in (y:ys), Retruns False otherwise
EXAMPLES: 
  letterchecker "abcdefn" "can" == True
  letterchecker "abcdefn" "hey" == False
  letterchecker "" "hello" == False
  letterchecker "hello" "" == True

--}
letterchecker :: Eq a => [a] -> [a] -> Bool
-- VARIANT: length (y:ys) or length (x:xs)
letterchecker [] [] = True
letterchecker (y:ys) [] = True
letterchecker [] (x:xs) = False
letterchecker (y:ys) (x:xs) = letterchecker (lettercheckerAux x (y:ys)) xs


{-- lettercheckerAux x (y:ys)
deletes the first x which is presented in (y:ys)
PRE: True
RETURNS: the rest of (y:ys) if x is an element in (y:ys), returns and empty string otherwise
EXAMPLES: 
  lettercheckerAux ' ' "hello" == ""
  lettercheckerAux  'h' "hello" == "ello"
  lettercheckerAux  'l' "hello" == "helo"
  lettercheckerAux 'y' "who" == ""
--}
lettercheckerAux :: Eq a => a -> [a] -> [a]
--VARIANT: length (y:ys)
lettercheckerAux _ [] = []
lettercheckerAux x (y:ys) =
  if x `elem` (y:ys) then
    if x == y then ys
    else y : lettercheckerAux x ys
  else []


{-- delete lst (y:ys)
deletes the elemnt in (y:ys) which are included in lst 
PRE: True
RETURNS: the rest of (y:ys) after the common elements between (y:ys) and lst have been removed
EXAMPLES:
   delete "he" "hello" == "llo"
   delete "hello" "he" == ""
   delete "" "word" == "word"

--}
delete :: Eq a => [a] -> [a] -> [a]
--VARIANT: lenght lst or length (y:ys)
delete _ [] = []
delete [] (y:ys) = y:ys
delete lst (y:ys)
  | head lst == y = delete (tail lst) ys
  | otherwise = y : delete lst ys


{--pointscounter counts the points of the player

--}
pointscounter :: [Char] -> Int -> Int
pointscounter [] acc = acc
pointscounter (x:xs) acc = conv (Ta.lookup tableOfPoints x) + pointscounter xs acc
  where conv (Just x) = x