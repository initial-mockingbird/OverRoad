import           Data.Char (toLower)
--------------------------------------------------
{- Data type declaration and global constants. -}
--------------------------------------------------

-- Some types for better understanding.
-- Components of a Sandwich
-- | Every Sandwhich starts off with a bread.
type Bread = String
-- | As a filling, a Sandwich might have veggies on it.
type Veggie = String
-- | It's also desirable to have some sauces to top everything off.
type Sauce = String
-- | For the filling, we want to offer our vegetarian friends
-- an option, therefore we allow the filling to have an exclusive option
-- for vegetarians.
data Filling = Veggie [Veggie] | Mixture [String] deriving (Eq,Show)


-- | Finally, we declare a Sandwhich as something that has
-- a bread, some filling, an of course, a list of sauces.
data Sandwich = Sandwich
  {
  breadType :: Bread,
  fill      :: Filling,
  saucing   :: [Sauce]
  } deriving (Eq)

instance Show Sandwich where
  show s = frame $
           "Bread Type: " ++ breadType s      ++ "\n" ++
           "Filling: "    ++ show (fill s)    ++ "\n" ++
           "Sauces: "     ++ show (saucing s)

-- | List of all the bread options
breadOptions :: [Bread]
breadOptions = ["wraps","oil-based loave","sourdough"]

-- | List of all the non-veggie fillings.
nonVeggieOpts :: [String]
nonVeggieOpts = ["chicken", "ham","bacon","beef","shrimps", "cheese","salami"]

-- | List of all the veggie fillings
veggieOpts :: [Veggie]
veggieOpts = ["lettuce","tomato","red onion", "caramelized onion", "white onion",
              "pickles"]

-- | List of all the sauces
sauceOpts :: [Sauce]
sauceOpts = ["bbq","cool ranch","mayo"]

-- | List of mixed fillings
mixed :: [String]
mixed = veggieOpts ++ nonVeggieOpts

---------------------
{- Main Functions -}
---------------------

sandwichChoices :: [Bread] -> [Filling] -> [[Sauce]] -> [Sandwich]
sandwichChoices breads fillings sauces = Sandwich <$> breads           <*> fillings <*> sauces
--                                         ^            ^                     ^           ^
--                                    We map the        ^                     ^           ^
--                                    Sandwhich         ^                     ^           ^
--                                    constructor       ^                     ^           ^
--                                                      ^                     ^           ^
--                                                 To each bread              ^           ^
--                                                 to form something          ^           ^
--                                                 like:                      ^           ^
--                                                 [Sandwich bread1,          ^           ^
--                                                  Sandwich bread2,          ^           ^
--                                                  ...]. Notice              ^           ^
--                                                 That Sanwich bread         ^           ^
--                                                 is still a function        ^           ^
--                                                 that has to take a         ^           ^
--                                                 filling and a sauce        ^           ^
--                                                 to produce a proper        ^           ^
--                                                 sandwich.                  ^           ^
--                                                                            ^           ^
--                                                                            |___________|
--                                                                                  |
--                                                                                  v
--                                                                          Finally be combine
--                                                                          each element in the list
--                                                                          [Sandwich bread] with each
--                                                                          possible filling and each
--                                                                          possible sauce, this is
--                                                                          possible since <*> applies
--                                                                          each function in a list
--                                                                          to each value in another list.


main :: IO ()
main = do
  putStrLn $ frame "Welcome to OverRoad"

  fillingInput <- putStr "Would you like to see an exclusive list of vegetarian options for your filling? (Y/N) " >> getLine

  putStrLn "Currently, we have the following ingredients:"
  putStrLn $ if (map toLower fillingInput) !! 0 == 'y' then prettyPrintList veggieOpts else prettyPrintList mixed
  putStrLn "If you would like to beto some ingredients, please do so using the following format: index of item1 index of item 2..."
  putStrLn "Eg: 1 5 3"

  betos <- getLine

  userInput <- (putStr "Would you like to limit the number of fillings (Y/N): " >> getLine)

  numberOfFillings <- case (map toLower userInput) !! 0 of
    'y' -> putStr "Please input the number of fillings you would like to consider: " >> getLine
    _ -> (return . show . length) mixed

  putStrLn "Please select the sauces you would like to consider (do so using the following format: index of item1 index of item 2...): "
  putStrLn "Eg: 1 5 3"
  putStrLn $ prettyPrintList sauceOpts

  saucesToConsider <- getLine

  printFilteredSandwich fillingInput betos


  return ()


--------------------
{- Aux Functions -}
--------------------
-- | Given a list of printable objects, yields an enumerated list with
-- the string representation of each object.
prettyPrintList :: Show a => [a] -> String
prettyPrintList list = concatMap printItem (zip [1..] list) where
  --                      ^          ^             ^
  --                      ^          ^      We zip each element with a number
  --                      ^          ^
  --                      ^ then we print each item
  --                      ^
  --                  and we combine the results using concat.
  -- Print item just tells you how each item is printed.
  printItem = \(index,item) -> show index ++ ") " ++ show item ++ "\n"

-- | Frames a text. Eg:
--                                ---------------------
-- frame "Welome to OverRoad" =  | Welcome to OverRoad |
--                                ---------------------
frame :: String -> String
frame s = line ++ "\n" ++  paddedString ++ line where
  -- The length of the top and bottom line must equal to the length of
  -- the max line + 2 since we are adding a little pad to the sides
  line = " " ++ replicate (maxLine + 2) '-'
  -- We get the text by lines
  wholeTextByLines = lines s
  -- We get the length of the max line
  maxLine = maximum $ map length wholeTextByLines
  -- Finally, we add the | line | pad.
  paddedString = concatMap (\s -> "| " ++ s ++ " |\n") wholeTextByLines

-- | Gets the n-combinations of a  set of elements.
combs :: Int -> [a] -> [[a]]
-- The 0-combinations of a set is the set that contains
-- the empty set.
combs 0 _        = [[]]
-- If the set is empty, then we return empty.
combs _ []       = []
--
combs n (x : xs) = map (x :)         (combs (n - 1) xs) ++        combs n xs
--                       ^                    ^                        ^
--                Else we append              ^                        ^
--                the head element            ^                        ^
--                      'x'                   ^                        ^
--                                            ^                        ^
--                                   To every combination              ^
--                                   of size n-1, in order             ^
--                                   to form combinations              ^
--                                   of size n that contains           ^
--                                   'x'                               ^
--                                                                     ^
--                                                         And finally, we must also consider
--                                                         the n-combinations that does not
--                                                         contain x. That is, we apply the
--                                                         same function to the rest of the list
--                                                         (notice that the rest of the list does
--                                                         not contain 'x').
