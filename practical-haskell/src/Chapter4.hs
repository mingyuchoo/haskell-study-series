--------------------------------------------------------------------------------
module Chapter4
  where
--------------------------------------------------------------------------------
import           Data.Graph
import qualified Data.Map   as M
import qualified Data.Set   as S
import           Data.Tree
--------------------------------------------------------------------------------
-- Map
--
-- >>> import qualified Data.Map as M
-- >>> M.empty
-- fromList []
-- >>> M.singleton "hello" 3
-- fromList [("hello",3)]
-- >>> M.fromList [("hello",1),("bye",2),("hello",3)]
-- fromList [("bye",2),("hello",3)]
-- >>> :{
-- >>> let m1 = M.singleton      "hello" 3
-- >>>     m2 = M.insert         "bye"   2 m1
-- >>>     m3 = M.insert         "hello" 5 m2
-- >>>     m4 = M.insertWith (+) "hello" 7 m3
-- >>> in  (m1,m2,m3,m4)
-- >>> :}
-- >>> M.null M.empty
-- True
-- >>> let m = M.fromList [("hello",3),("bye",4)]
-- >>> M.null m
-- False
-- >>> M.member "hello" m
-- True
-- >>> M.lookup "hello" m
-- Just 3
-- >>> M.lookup "welcome" m
-- Nothing
-- >>> M.findWithDefault 0 "welcome" m
-- 0
-- >>> M.delete "hello" m
-- fromList [("bye",4)]
-- >>> M.adjust (+7) "hello" m
-- fromList [("bye",4),("hello,10)]
-- >>> M.alter (\(Just v) -> Just (v + 7)) "hello" m
-- fromList [("bye",4),("hello,10)]
-- >>> :{
-- >>> let m1 = M.fromList [("hello",3),("bye",4)]
-- >>>     m2 = M.fromList [("hello",5),("welcome",6)]
-- >>> in  (m1 `M.union` m2, M.intersectionWith (-) m1 m2)
-- >>> :}
-- >>> (M.map (* 2) m, M.foldr (+) 0 m)
-- (fromList [("bye",8),("hello",6)],7)
--------------------------------------------------------------------------------
-- Set
--
-- >>> S.insert "welcome" $ S.singleton "hello"
-- fromList ["hello","welcome"]
-- >>> S.fromList ["hello","bye","hello"]
-- fromList ["bye","hello"]
-- >>> S.toList $ S.fromList ["duplicate","boom","duplicate"]
-- ["boom","duplicate"]
-- >>> :{
-- >>> let set1 = S.insert "welcome" $ S.singleton "hello"
-- >>>     set2 = S.fromList ["hello","bye"]
-- >>> in ( set1 `S.intersection` set2
-- >>>    , "welcome" `S.member` set1
-- >>>    , S.map length set2
-- >>>    )
-- >>> :}
-- (fromList ["hello],True,fromList [3,5])
--------------------------------------------------------------------------------
-- Tree
--

-- | preOrder
--
-- >>> preOrder show $ Node 1 []
-- ["1"]
preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subtrees) =
  let subtreesTraversed = concat $ map (preOrder f) subtrees
  in f v : subtreesTraversed

-- | pictureTree
--
-- >>> preOrder show pictureTree
-- ["1","2","3","4","5","6"]
-- >>> flatten pictureTree
-- [1,2,3,4,5,6]
-- >>> levels pictureTree
-- [[1],[2,6],[3,4,5]]
pictureTree :: Tree Int
pictureTree = Node 1 [ Node 2 [ Node 3 []
                              , Node 4 []
                              , Node 5 []]
                              , Node 6 []]

-- | fmap (predicate) tree
--
-- >>> fmap (*2) pictureTree
-- Node {rootLabel = 2, subForest = [Node {rootLabel = 4, subForest = [Node {rootLabel = 6, subForest = []},Node {rootLabel = 8, subForest = []},Node {rootLabel = 10, subForest = []}]},Node {rootLabel = 12, subForest = []}]}


-- | Data.Foldable.foldr (predicate) initial tree
--
-- >>> Data.Foldable.foldr (+) 0 pictureTree
-- 21

--------------------------------------------------------------------------------
-- Graph
--

-- | timeMachineGraph  ( is just value)
--
timeMachineGraph :: [(String, String, [String])]
timeMachineGraph = [ ("wood",     "wood",     ["walls"])
                   , ("plastic",  "plastic",  ["walls",  "wheels"])
                   , ("aluminum", "aluminum", ["wheels", "door"])
                   , ("walls",    "walls",    ["done"])
                   , ("wheels",   "wheels",   ["done"])
                   , ("door",     "door",     ["done"])
                   , ("done",     "done",     [])
                   ]

-- | tmeMachinePrecence
--
timeMachinePrecedence :: (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)
timeMachinePrecedence = graphFromEdges timeMachineGraph

-- | timeMachineTravel
-- | path      :: Graph -> Vertex -> Vertex -> Bool
-- | buildG    :: Bounds -> [Edge] -> Graph
-- | reachable :: Graph -> Vertex -> [Vertex]
--
-- >>> path timeMachineTravel 1302 917
-- True
-- >>> reachable timeMachineTravel 1302
-- [1302,2013,1408,917,103,1993,1614]
timeMachineTravel :: Graph
timeMachineTravel = buildG (103,2013) [ (1302,1614)
                                      , (1614,1302)
                                      , (1302,2013)
                                      , (2013,1302)
                                      , (1614,2013)
                                      , (2013,1408)
                                      , (1408,1993)
                                      , (1408,917)
                                      , (1993,917)
                                      , (917, 103)
                                      , (103, 917)
                                      ]


--------------------------------------------------------------------------------
--
-- Ad-Hoc Polymorphism: type Classes
--
--------------------------------------------------------------------------------
-- Declaring Classes and Instances
--

-- (o) :kind Color  :: *       -- `Color`  is a Type Constructor and a Data Type
-- (o) :type Red    :: Color   -- `Red`    is a Data Constructor and a funciton
-- (o) :type Yellow :: Color   -- `Yellow` is a Data Constructor and a funciton
-- (o) :type Green  :: Color   -- `Green`  is a Data Constructor and a funciton
-- (o) :type Blue   :: Color   -- `Blue`   is a Data Constructor and a funciton
data Color = Red
           | Yellow
           | Green
           | Blue

-- (o) :kind Contrast  :: * -> *              -- `Contrast` is a Type Constructor and a Data Type
-- (o) :type White     :: i -> Contrast i     -- `White`    is a Data Constructor and a function
-- (x) :type White Int :: << error >>         -- This expression cant's construct a data (or a value).
-- (o) :type White 1   :: Num i => Contrast i -- This is correct, it's a function call
-- (o) :type Black     :: i -> Contrast i     --`Black`     is a Data Constructor and a function
-- (x) :type Black String :: << error >>      -- This expression cant's construct a data (or a value).
-- (o) :type Black "000"  :: Contrast [Char]  -- This is correct, it's a function call
data Contrast i = White i -- `i` is a type variable
                | Black i

-- | Nameable type class
-- (o) :kind Nameable        :: * -> Constraint
-- (o) :kind Nameable Int    :: Constraint
-- (o) :kind Nameable String :: Constraint
-- (o) :kind Nameable Color  :: Constraint
-- (x) :kind Nameable Contrast       :: << error >> -- This expression cant's construct a type.
-- (x) :kind Nameable (Contrast i)   :: << error >> -- This expression cant's construct a type.
-- (o) :kind Nameable (Contrast Int) :: Constraint
class Nameable t where  -- `t` is a type
  name :: t -> String

-- | initial
--
initial :: Nameable t => t -> Char
initial t = head (name t)


-- | instance Nameable Color
--
instance Nameable Color where -- Color is a type
  name Red    = "Red"
  name Yellow = "Yellow"
  name Green  = "Green"
  name Blue   = "Blue"


-- | instance Nameable Contrast
--
instance Nameable (Contrast i) where -- (Contrast i) is a type, `i` is a type varialble
  name (White i) = "White" -- `i` should be a value of a type variable
  name (Black i) = "Black" -- `i` should be a value of a type variable


--------------------------------------------------------------------------------
-- Built-in Type Classes
--

