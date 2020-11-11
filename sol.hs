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
  show s = "Bread Type: " ++ breadType s      ++ "\n" ++
           "Filling: "    ++ show (fill s)    ++ "\n"  ++
           "Sauces: "     ++ show (saucing s) ++ "\n"

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

--------------------
{- Aux Functions -}
--------------------

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
