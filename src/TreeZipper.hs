{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module TreeZipper
    ( TreeZipper
    , treeZipper
    , getFocused
    , modify
    , goUp
    , goDownBy
    ) where

import           Data.Binary   (Binary)
import           Data.Foldable (find)
import           Data.Tree     (Tree (Node, rootLabel, subForest))
import           GHC.Generics  (Generic)

data TreeCrumb a = TreeCrumb a [Tree a] deriving (Show, Ord, Eq, Generic)

instance (Binary a) => Binary (TreeCrumb a)

type TreeZipper a = (Tree a, [TreeCrumb a])

treeZipper :: Tree a -> TreeZipper a
treeZipper t = (t, [])

getFocused :: TreeZipper a -> a
getFocused (t, _) = rootLabel t

modify :: (a -> a) -> TreeZipper a -> TreeZipper a
modify f (n@Node { rootLabel = r }, bs) = (n { rootLabel = f r }, bs)

goUp :: TreeZipper a -> Maybe (TreeZipper a)
goUp (Node { rootLabel = r, subForest = fs }, TreeCrumb newRootLabel newSubForest:bs) =
    Just (Node { rootLabel = newRootLabel, subForest = Node r fs:newSubForest }, bs)
goUp (_, []) = Nothing

goDownBy :: (a -> Bool) -> TreeZipper a -> Maybe (TreeZipper a)
goDownBy f (Node { rootLabel = r, subForest = ts }, bs) =
    (, TreeCrumb r ts:bs) <$> find (f . rootLabel)  ts
