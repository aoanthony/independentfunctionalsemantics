module Main where
import Data.List

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

ll SnowWhite  = E_SnowWhite
ll Alice      = E_Alice
ll Laughed    = E_Laughing
ll Loved      = E_Loving
ll Admired    = E_Admiring
ll Gave       = E_Giving


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


memberTree :: Eq a => a -> Tree a -> Bool
memberTree x (Leaf y) | x == y    = True
                      | otherwise = False
memberTree a (Branch y z) =  (memberTree a y) || (memberTree a z)



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




-- exercises: games from van Eijk & Unger

-- battleship: 2x 10x10 grids identified by letter & number

-- battleship: 5, frigate: 4, two submarines : 3, destroyer : 2

data Column      = A | B | C | D | E | F | G | H | I | J deriving (Eq,Ord,Show,Enum)
type Row         = Integer
type Attack      = Pos
data Ship        = Battleship | Frigate | Submarine | Destroyer deriving Show
data Reaction    = Missed | Hit Ship | Sunk Ship | Lost deriving Show
type Turn        = (Attack, Reaction)
type Pos         = (Column, Row)
type Grid        = [Pos]
type ActionState = (Grid,Grid)


type BSState   = (Pos, (Pos, (Pos, (Pos, Pos))))
type FRState   = (Pos, (Pos, (Pos, (Pos))))
type SUState   = (Pos, (Pos, Pos))
type DSState   = (Pos, Pos)
type ShipState = (BSState, (FRState, (SUState, DSState)))


--excludes actionstates where first Grid [hits] has no overlap with second grid [misses]
properAS :: ActionState -> Bool
properAS (x,y) | null (x `intersect` y)  = True
               | otherwise               = False

-- rules out ShipStates w/overlapping ships
-- rules out ShipStates w/
properGame :: ShipState -> Bool
properGame state = not (repeats (ssFringe state))
                 && contiguousBS (fst state)
                 && contiguousFR (fst (snd state))
                 && contiguousSU (fst (snd (snd state)))
                 && contiguousDS (snd (snd (snd state)))

contiguousBS :: BSState -> Bool
contiguousBS (x1, (x2, (x3, (x4, x5)))) |     adjacent x1 x2
                                          &&  adjacent x2 x3
                                          &&  adjacent x3 x4
                                          &&  adjacent x4 x5 = True
                                        | otherwise          = False


contiguousFR :: FRState -> Bool
contiguousFR (x1, (x2, (x3, x4))) |  adjacent x1 x2
                                     &&  adjacent x2 x3
                                     &&  adjacent x3 x4 = True
                                  |  otherwise          = False


contiguousSU :: SUState -> Bool
contiguousSU (x1, (x2, x3))       |  adjacent x1 x2
                                     && adjacent x2 x3 = True
                                  |  otherwise         = False


contiguousDS :: DSState -> Bool
contiguousDS (x1, x2)             |  adjacent x1 x2    = True
                                  |  otherwise         = False


-- Define ShipState
defineLocation :: BSState -> FRState -> SUState -> DSState -> ShipState
defineLocation x1 x2 x3 x4 = (x1, (x2, (x3, x4)))


-- works! don't change
getReaction :: Attack -> ShipState -> Reaction
getReaction x y | x `elem` bs5Fringe (fst y)         = Hit Battleship
                | x `elem` fr4Fringe (fst (snd y))   = Hit Frigate
                | x `elem` su3Fringe (fst
                                     (snd (snd y)))  = Hit Submarine
                | x `elem` ds2Fringe (snd
                                     (snd (snd y)))  = Hit Destroyer

adjacent :: Pos -> Pos -> Bool
adjacent (x,y) (z,a) | x < z && succ x == z && y == a  = True
                     | x > z && pred x == z && y == a  = True
                     | y < a && succ y == a && x == z  = True
                     | y > a && pred y == a && x == z  = True
                     | otherwise                       = False


-- Get ShipState Fringe
ssFringe :: ShipState -> [Pos]
ssFringe (bs,(fr,(su,ds))) = bs5Fringe bs ++ fr4Fringe fr ++ su3Fringe su ++ ds2Fringe ds

-- does the ssFringe have multiple entites in the same position?
repeats :: [Pos] -> Bool
repeats (x:xs) | x `elem` xs = True
              | otherwise   = repeats xs
repeats _                    = False


--defineLocation :: ((Ship, BSState), (Ship, FRState), (Ship, SUState), (Ship, DSState)) -> ShipState
--defineLocation x = map4Tuple snd x



-- HELPER FUNCTIONS

-- 5ples
bs5Maker :: Pos -> Pos -> Pos -> Pos -> Pos -> (Pos, (Pos, (Pos, (Pos, Pos))))
bs5Maker x1 x2 x3 x4 x5 = (x1,(x2,(x3,(x4, x5))))

bs5Fringe :: (Pos, (Pos, (Pos, (Pos, Pos)))) -> [Pos]
bs5Fringe (x1,(x2,(x3,(x4,x5)))) = [x1,x2,x3,x4,x5]

-- 4ples
fr4Maker :: Pos -> Pos -> Pos -> Pos -> (Pos,(Pos,(Pos,Pos)))
fr4Maker x1 x2 x3 x4 = (x1,(x2,(x3,x4)))

fr4Fringe :: (Pos, (Pos, (Pos, Pos))) -> [Pos]
fr4Fringe (x1,(x2,(x3,x4))) = [x1,x2,x3,x4]

-- 3ples
su3Maker :: Pos -> Pos -> Pos -> (Pos,(Pos,Pos))
su3Maker x1 x2 x3 = (x1,(x2,x3))

su3Fringe :: (Pos, (Pos, Pos)) -> [Pos]
su3Fringe (x1,(x2,x3)) = [x1,x2,x3]

-- 2ples
ds2Maker :: Pos -> Pos -> (Pos,Pos)
ds2Maker x y = (x,y)

ds2Fringe :: (Pos, Pos) -> [Pos]
ds2Fringe (x1,x2) = [x1,x2]




-- defines valid ShipState w/ no duplicate Poss


bss  = ((A,1),
            ((B,1),
              ((C,1),
                ((D,1),(E,1)))))
frs  = ((A,2),((B,2),((C,2),(D,2))))
sus  = ((A,3),((B,3),(C,3)))
dss  = ((A,4),(B,4))

ss  = (bss,(frs,(sus,dss)))














a1 = (A,1)
a2 = (A,2)
a3 = (A,3)
a4 = (A,4)
a5 = (A,5)
a6 = (A,6)
b1 = (B,1)
b2 = (B,2)
b3 = (B,3)
b4 = (B,4)
b5 = (B,5)
b6 = (B,6)



--fringeTuple :: ShipState -> Grid









-- locate :: Ship -> [Pos]
-- locate Battleship = [(A,1), (B,1), (C,1), (D,1), (E,1)]
-- locate Frigate = [(A,2), (B,2), (C,2) ]


--seems to work



--occupied :: Pos -> Bool
