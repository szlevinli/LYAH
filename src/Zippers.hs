{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Zippers
  ( freeTree,
    changeToP,
    elemAt,
    goLeft,
    goRight,
    (-:),
    goLeft',
    goRight',
    goUp,
    modify,
    attach,
    topMost,
    goForward,
    goBack,
    myDisk,
    fsUp,
    nameIs,
    fsTo,
    fsRename,
    fsNewFile,
    goLeftM,
    goRightM,
    goUpM,
  )
where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
  Node
    'P'
    ( Node
        'O'
        ( Node
            'L'
            (Node 'N' Empty Empty)
            (Node 'T' Empty Empty)
        )
        ( Node
            'Y'
            (Node 'S' Empty Empty)
            (Node 'A' Empty Empty)
        )
    )
    ( Node
        'L'
        ( Node
            'W'
            (Node 'C' Empty Empty)
            (Node 'R' Empty Empty)
        )
        ( Node
            'A'
            (Node 'A' Empty Empty)
            (Node 'C' Empty Empty)
        )
    )

data Direction = L | R deriving (Show)

type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L : ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R : ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

{-
To avoid printing out the whole tree, let's make a function that takes a list
of directions and tells us what the element at destination is:
-}

elemAt :: Directions -> Tree a -> a
elemAt [] (Node x _ _) = x
elemAt (L : ds) (Node _ l _) = elemAt ds l
elemAt (R : ds) (Node _ _ r) = elemAt ds r

{-
While this technique may seem cool, it can be rather inefficient, especially if
we want to repeatedly change elements.

In the next section, we'll find a better way of focusing on a sub-tree, one that
allow use to efficiently switch focus to sub-trees that are nearly.
-}

--
-- A trail of breadcrumbs
--

{-
That is, when we go left, we remember that we went left and when we go right,
we remember that we went right.
-}

type Breadcrumbs = [Direction]

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L : bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R : bs)

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

{-
Going back up

What if we now want to go back up in our tree? From our breadcrumbs we know that
the current tree is the left sub-tree of it parent and that it is right sub-tree
of its parent, but that's it. They don't tell us enough about the parent of the
current sub-tree for us to be able to go up in the tree. It would seem that apart
from the direction that we took, a single breadcrumb should also contain all other
data that we need to go back up.
-}

-- change `Direction` to `Crumb`
-- data Direction = L | R deriving (Show)
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs' a = [Crumb a]

goLeft' :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goLeft' (Node x l r, bs) = (l, LeftCrumb x r : bs)

goRight' :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goRight' (Node x l r, bs) = (r, LeftCrumb x l : bs)

goUp :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goUp (t, LeftCrumb x r : bs) = (Node x r t, bs)
goUp (t, RightCrumb x l : bs) = (Node x t l, bs)

{-
Zipper

Such a pair that contains a focused part of a data structure and its surrounding
is called a `Zipper`, because moving our focus up and down the data structure
resembles the operation of a zipper on a regular pair of pants.
-}

type Zipper a = (Tree a, Breadcrumbs' a)

{-
Manipulating trees under focus

Now that we can move up and down, let's make a function that modifies the element
in the root of the sub-tree that the zipper is focusing on:

If we're focusing on a node, we modify its root element with the function `f`.
If we're focusing on an empty tree, we leave it as it is.
-}

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ (Empty, bs) = (Empty, bs)

-- Attach sub-tree to empty leaf node
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

--
-- Focusing on lists
--

data List a = LEmpty | Cons a (List a) deriving (Show, Read, Eq, Ord)

{-
Contrast thi with our definition of our binary tree and it's easy to see how lists
can be viewed as trees where each node has only one sub-tree.

A list like `[1, 2, 3]` can be written as `1:2:3:[]`. It consists of the
head of the list, which is `1` and the the list's tail, which is `2:3:[]`.
In turn, `2:3:[]` also has a head, which is `2` and a tail, which is `3:[]`.
With `3:[]`, the `3` is the head and the tail is the empty list `[]`.
-}

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x : xs, bs) = (xs, x : bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b : bs) = (b : xs, bs)

--
-- A very simple file system
--

type Name = String

type Data = String

data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

-- cspell: disable
myDisk :: FSItem
myDisk =
  Folder
    "root"
    [ File "goat_yelling_like_man.wmv" "baaaaaa",
      File "pope_time.avi" "god bless",
      Folder
        "pics"
        [ File "ape_throwing_up.jpg" "bleargh",
          File "watermelon_smash.gif" "smash!!",
          File "skull_man(scary).bmp" "Yikes!"
        ],
      File "dijon_poupon.doc" "best mustard",
      Folder
        "programs"
        [ File "fartwizard.exe" "10gotofart",
          File "owl_bandit.dmg" "mov eax, h00t",
          File "not_a_virus.exe" "really not a virus",
          Folder
            "source code"
            [ File "best_hs_prog.hs" "main = print (fix error)",
              File "random.hs" "main = print 4"
            ]
        ]
    ]

-- cspell: enable

{-
In this case, a breadcrumb should be like a folder, only it should be missing the
folder that we currently chose. Why not like a file, you ask? Well, because once
we're focusing on a file, we con't move deeper into the file system, so it doesn't
make sense to leave a breadcrumb that says that we came from a file. A file is sort
of like an empty tree.
-}

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

--                      ^      ^        ^
--                      |      |        |-> the items come after the file that we're focusing on
--                      |      |-> the items come before the file that we're focusing on
--                      |-> parent folder name that we're focusing on

type FSZipper = (FSItem, [FSCrumb])

{-
Because our breadcrumb knew what the parent folder's name war, as well as the items
that come before our focused item in the folder (that's `ls`) and the ones that
came after (that's `rs`), moving up was easy.
-}
fsUp :: FSZipper -> FSZipper
fsUp (item, (FSCrumb name ls rs) : bs) = (Folder name (ls ++ [item] ++ rs), bs)

{-
`fsTo` takes a `Name` and a `FSZipper` and return a new `FSZipper` that focuses
on the file with the given name. That file has to be in the current focused folder.
This function doesn't search all over the place, it just looks at the current folder.
-}
fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
  let (ls, item : rs) = break (nameIs name) items
   in (item, FSCrumb folderName ls rs : bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

{-
Manipulating our file system

Now that we know how to navigate our file system, manipulating it is easy.
Here's a function that rename the currently focused file or folder.
-}

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder _ items, bs) = (Folder newName items, bs)
fsRename newName (File _ items, bs) = (File newName items, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = (Folder folderName (item : items), bs)

--
-- Watch your step
--

goLeftM :: Zipper a -> Maybe (Zipper a)
goLeftM (Node x l r, bs) = Just (l, LeftCrumb x r : bs)
goLeftM (Empty, _) = Nothing

goRightM :: Zipper a -> Maybe (Zipper a)
goRightM (Node x l r, bs) = Just (r, RightCrumb x l : bs)
goRightM (Empty, _) = Nothing

goUpM :: Zipper a -> Maybe (Zipper a)
goUpM (t, LeftCrumb x r : bs) = Just (Node x t r, bs)
goUpM (t, RightCrumb x l : bs) = Just (Node x l t, bs)
goUpM (_, []) = Nothing