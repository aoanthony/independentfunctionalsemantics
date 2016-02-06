module Main where
main :: IO()
main = do
  putStrLn "hello here"



data PrimitiveExpressions = The | Every | Some | No  --DET
                          | Girl | Boy | Hero        --CN
                          | SnowWhite | Alice        --N
                          | Laughed                  --BVP
                          | Loved | Admired          --TV
                          | Gave                     --DV
                          deriving (Eq, Show, Ord)

data PrimitiveTypes       = DET | CN | N | IV | TV | DV deriving (Eq, Show, Ord)

data PrimitiveDenotations = E_SnowWhite | E_Alice | E_John | E_Mary
                          | E_Laughing | E_Loving | E_Admiring | E_Giving
                          deriving (Eq, Show, Ord)
--ll    "doubleBrackets" [[ ]] primitiveinterp

ll :: PrimitiveExpressions -> PrimitiveDenotations

ll SnowWhite = E_SnowWhite
ll Alice = E_Alice
ll Laughed = E_Laughing
ll Loved = E_Loving
ll Admired = E_Admiring
ll Gave = E_Giving


data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show, Eq)

gave = Leaf (Gave, DV)
gavei = Leaf (Gave, CN)
boy  = Leaf (Boy, CN)
boyi = Leaf (Boy, DV)
alice = Leaf (Alice, N)
laughed = Leaf (Laughed, IV)
alicelaughed = Branch laughed alice
alice2 = Branch alicelaughed laughed

treeExpr tree = treeMap fst tree
treeDeno tree = treeMap ll $ treeMap fst tree
treeType tree = treeMap snd tree

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Branch x y) = Branch (treeMap f x) (treeMap f y)

treeFold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
treeFold fbranch fleaf = g where
  g (Leaf x) = fleaf x
  g (Branch left right) = fbranch (g left) (g right)

fringeTree = treeFold (++) (: [])


typeOf :: PrimitiveExpressions -> PrimitiveTypes

typeOf The        = DET
typeOf Every      = DET
typeOf Some       = DET
typeOf No         = DET
typeOf Girl       = CN
typeOf Boy        = CN
typeOf Hero       = CN
typeOf SnowWhite  = N
typeOf Alice      = N
typeOf Laughed    = IV
typeOf Loved      = TV
typeOf Admired    = TV
typeOf Gave       = DV



cons = consTree
consTree x y = [x]:[y]




type Value = (PrimitiveExpressions, PrimitiveTypes)

proper :: Value -> Bool
proper node | typeOf (fst node) == snd node   = True
            | otherwise                       = False

--data Tree a = Leaf a | Branch (Tree a) (Tree a)


member :: Eq a => a -> [a] -> Bool
member _ [] = False
member a (x:xs) | a == x      = True
                | otherwise   = member a xs


rember :: Eq a => a -> [a] -> [a]
rember _ [] = []
rember a (x:xs) | a == x      = xs
                | otherwise   = x : rember a xs

multirember :: Eq a => a -> [a] -> [a]
multirember _ [] = []
multirember a (x:xs) | a == x      = multirember a xs
                     | otherwise   = x : multirember a xs


insertR :: Eq a => a -> a -> [a] -> [a]

insertR new old [] = []
insertR new old (x:xs) | old == x     = old : (new : xs)
                       | otherwise    = x : insertR new old xs


-- consTree 1 Branch Empty Empty =
-- Branch Tree (Branch Empty Empty)

-- data Tree a = Node {rootLabel::a,
--                     subForest::Forest a}
-- types Forest a = [Tree A]

--"Leaf" is an empty tree case
-- cons = consTree
-- consTree :: Value -> Tree.Tree Value -> Tree.Tree Value
-- consTree x Tree.fail "x"

-- insert' :: Ord a => a -> Tree a -> Tree a
-- insert' b Empty = Node Empty b Empty
-- insert' b (Node left a right)
--   | b == a    = Node left a right
--   | b < a     = Node (insert' b left) a right
--   | b > a     = Node left a (insert' b right)



-- now make trees outta proper nodes



-- data Mood = Blah | Woot deriving (Show, Eq)
--
-- changeMood :: Mood -> Mood
-- changeMood x  | x == Blah   = Woot
--               | x == Woot   = Blah
--
--
--
-- type Rel a = [(a,a)]
--
--
-- data DET  = The | Every | Some | No deriving (Show, Eq)
-- data CN   = Girl | Boy | Hero | Giant | Wizard deriving (Show, Eq)
-- data NP   = SnowWhite | Alice | Dorothy | Goldilocks
--           | LittleMook | Atreyu | Everyone | Someone
--           | NP1 DET CN
--           deriving (Show, Eq)
--
--
-- data VP   = Bare BVP | DV NP NP | TV NP deriving (Show, Eq)
-- data BVP  = Laughed | Cheered deriving (Show,Eq)
-- data TV   = Loved | Admired | Helped | Defeated | Caught deriving (Show, Eq)
-- data DV   = Gave deriving (Show, Eq)
--
--
-- data EXP  = VP VP | NP NP | Sent Sent deriving (Show, Eq)
--
-- express :: EXP -> IO()
-- express expr = do
--   print expr
--   print expr
--
-- type Node = (VP, EVE)
--
-- nodeEval :: Node -> Bool
-- nodeEval (Bare Laughed, Laughing []) = False
-- nodeEval (_,_) = True
--
-- type Sent = S
-- data S = S NP VP deriving (Show, Eq)
--
--
-- data ENT  = John | Mary | Gift
-- data EVE  = Laughing [ENT] | Cheering ENT | LOVING ENT ENT | ADMIRING ENT ENT | HELPING ENT ENT | Defeating [ENT] [ENT] | CATCHING ENT ENT | GIVING ENT ENT ENT
--
--
-- denotationNP :: NP -> ENT
-- denotationNP (NP1 The Boy)  = John
-- denotationNP (NP1 The Girl) = Mary
-- denotationNP SnowWhite      = Mary


-- Works

-- *Main> :t (The Girl)
-- <interactive>:1:2:
--     Couldn't match expected type ‘CN -> t’ with actual type ‘DET’
--     The function ‘The’ is applied to one argument,
--     but its type ‘DET’ has none
--     In the expression: (The Girl)


-- *Main> :t (NP1 The Girl)
-- (NP1 The Girl) :: NP

-- *Main> :t (S (NP1 The Girl) Laughed)
-- (S (NP1 The Girl) Laughed) :: S


-- intermediate goal: diplay as tree of pairs like (The, DET)

-- myFunc (S (NP1 The Girl) Laughed) =
--                                   S (NP1 (The, DET) (Girl, CN)) (Laughed, VP)




-- goal: print all the things using recursion
--
-- challenge: do it both "bottom up" and "top down"
