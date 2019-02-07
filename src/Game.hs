--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Mastermind                                                   --
--------------------------------------------------------------------------------

-- | This module should contain your game code.
module Game where
import Data.List
--------------------------------------------------------------------------------

-- | The number of pegs in a code.
pegs :: Int
pegs = 4

-- | Symbols are represented by characters.
type Symbol = Char

-- | The available symbols in the game.
symbols :: [Symbol]
symbols = ['a'..'f']

-- | A code is represented by a list of symbols.
type Code = [Symbol]

-- | Guesses are scored using coloured and white markers. The first component
-- of the pair gives the number of coloured markers and the right component
-- gives the number of white markers.
type Score = (Int, Int)

-- | A player is either human or computer-controlled.
data Player = Human | Computer

-- | The first codemaker in a play session.
codemaker :: Player
codemaker = Human

-- | The first guess the AI will make.
firstGuess :: Code
firstGuess = "aabb"

--------------------------------------------------------------------------------

-- | Determines whether a score indicates that a guess was correct or not.
------------------------------------
--
correctGuess :: Score -> Bool
correctGuess score = (fst score) == pegs && (snd score) == 0

-- | This function should check that the code entered by a human player is
-- valid. In other words, it should have the length given by `pegs` and it
-- should only contain valid symbols.
------------------------------------
-- [Your explanation]
validateCode :: Code -> Bool
validateCode xs = length xs == pegs && all (`elem` symbols) xs

-- | All possible codes.
------------------------------------
-- [Your explanation]
codes :: [Code]
codes = filter validateCode $ sequence $ replicate pegs symbols

-- | All possible scores.
------------------------------------
-- [Your explanation]
results :: [Score]
results = filter(\(a,b) -> a /= pegs -1 || b /= 1) [(p,q) | p <- [0..pegs], q <- [0..pegs-p] ]

-- | Scores a guess against a code. Symbols which are in the right place
-- and of the right type score a coloured marker. Symbols which are of the
-- right type but in the wrong place score a white marker.
------------------------------------
-- [Your explanation]
score :: Code -> Code -> Score
score code guess = (coloured, white)
    where coloured = length [1 | (x,y) <- zip code guess, x==y]
          notFound = code \\ guess
          white = length code - length notFound - coloured


-- | Chooses the next guess. If there is only one option left, choose it.
-- Otherwise, calculate the hit score for each code and choose the code
-- with the largest hit score.
------------------------------------
-- [Your explanation]
nextGuess :: [Code] -> Code
nextGuess (s:ss)
    | length (s:ss) == 1 = s
    | otherwise = (snd . minimum) $ countAllEliminate (s:ss) codes


countAllEliminate :: [Code] -> [Code]-> [(Int, Code)]
countAllEliminate _ [] = []
countAllEliminate c (x:xs) = maximum [ (length $ eliminate r x c, x) | r <- results] : countAllEliminate c xs

-- | Remove all codes from the remaining possibilities which would result in
-- a different score for the guess if they were the code.
-- In other words, given the set of remaining possible codes, narrow it down
-- to those which would produce the same score we got from the codemaker.
------------------------------------
-- [Your explanation]
eliminate :: Score -> Code -> [Code] -> [Code]
eliminate lastScore guess codes =  filter (\code -> (score code guess) == lastScore) codes

--------------------------------------------------------------------------------
