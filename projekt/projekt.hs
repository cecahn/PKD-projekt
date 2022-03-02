import Control.Exception ( SomeException, catch, evaluate )
import Prelude hiding(catch)
import Data.Binary (Binary(putList))
import qualified Table as Ta
import Test.HUnit
import System.IO
import System.Random


listOfPoints = [('a',1), ('b',2), ('c',2), ('d',1), ('e',1), ('f',4), ('g',2), ('h',3), ('i',1), ('j',4), ('k',3), ('l',1), ('m',3), ('n',1), ('o',2), ('p',3), ('q',10), ('r',1), ('s',1), ('t',1), ('u',3), ('v',4), ('w',2), ('x',10), ('y',8), ('z',10)]
tableOfPoints = Ta.fromList listOfPoints

{-- main
starts the game by providing information and calling for other functions to continue the game
PRE: True
SIDEEFFECTS: prints information in the terminal
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
SIDEEFFECTS: True
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
SIDEEFFECTS: Prints strings depending on the player's input
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

readAnswer :: IO [Char]
readAnswer =
  catch (do
  line <- getLine
  evaluate (read line))
  ((\_ -> do
     putStrLn "Invalid input. Correct format: \"yes\"/\"no\" "
     putStrLn " "
     readAnswer) :: SomeException -> IO [Char])

{- continuePlay list acc char wordlist
reads the file english3 and checks the approved words against it. If the word is valid the function
stores the word, the last char, the players score and deletes the letters in the word from the players
list. 
PRE: True
SIDEEFFECTS: Prints strings depending on the word the player wrote
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
      let score = pointsCounter word acc
      putStrLn " "
      putStrLn "Here are all your words for this round: "
      print (word : wordlist)
      putStrLn " "
      putStrLn "Here is your current score: "
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
        putStrLn " "
        putStrLn "Here are all the words you created: "
        print (word : wordlist)
        putStrLn " "
        putStrLn "Here is your final score: "
        putStrLn " "
        print score
      else do
        putStrLn "Do you want to continue? (\"yes\"/\"no\") : "
        continue <- readAnswer
        if continue == "yes" then continuePlay newlist (pointsCounter word acc) nextfirst (word : wordlist)
        else do
          putStrLn " "
          putStrLn "Thank you for playing!"
          putStrLn " "
          putStrLn "Here are all the words you created: "
          print (word : wordlist)
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
RETURN: Different strings depending on what word the player wrote  
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
SIDEEFFECTS: True
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
SIDEEFFECTS: True
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
SIDEEFFECTS: True
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


{--pointsCounter (x:xs) acc
counts the points for each element from a list and adds it to the accumulator. 
PRE: True
RETURNS: acc, after each value corresponding to every x from (x:xs) in tableOfPoints is added to acc
EXAMPLES:
   pointsCounter "hello" 0 = 8
   pointsCounter "bye" 8 = 19
   pointsCounter "" 0 = 0
   
--}
pointsCounter :: [Char] -> Int -> Int
--VARIANT: lenght (x:xs)
pointsCounter [] acc = acc
pointsCounter (x:xs) acc = conv (Ta.lookup tableOfPoints x) + pointsCounter xs acc
  where conv (Just x) = x

---------------------------------------------------------------------------
--                            TESTCASES                                  --
---------------------------------------------------------------------------

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5, TestLabel "test6" test6, TestLabel "test7" test7, TestLabel "test8" test8, TestLabel "test9" test9]
test1 = TestCase $ assertEqual "letterchecker 'jeh' 'hej' " True  (letterchecker "jeh" "hej")
test2 = TestCase $ assertEqual "letterchecker 'kkkkkhmmmmjlllebb' 'hej'" True (letterchecker "kkkkkhmmmmjlllebb" "hej") 
test3 = TestCase $ assertEqual "letterchecker 'hej' 'da'" False (letterchecker "hej" "då")
test4 = TestCase $ assertEqual "pointscounter 'a' 0" 1 (pointsCounter "a" 0)
test5 = TestCase $ assertEqual "pointscounter [] 0" 0 (pointsCounter "" 0)
test6 = TestCase $ assertEqual "delete 'hej' 'kkjmmeddh" "kkjmmedd" (delete "hej" "kkjmmeddh")
test7 = TestCase $ assertEqual "delete 'm' 'llkkdd'" "llkkdd" (delete "m" "llkkdd")
test8 = TestCase $ assertEqual "letterchecker 'kkkjmmmme' 'hej'" False (letterchecker "kkkjmmme" "hej")
test9 = TestCase $ assertEqual "letterchecker 'mmmkkkk' ' ' " False (letterchecker "mmmkkk" "")


runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9]