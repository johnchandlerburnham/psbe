module FileOperations where

import Prelude

import Data.Path (Path, ls, isDirectory, size, root)
import Data.Maybe
import Data.Array (concatMap, (:), filter, elem)
import Data.Foldable (foldl)
import Control.MonadZero (guard)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles file = filter (not $ isDirectory) $ allFiles' file

onlyDirs :: Path -> Array Path
onlyDirs file = filter isDirectory $ allFiles' file

largestFile :: Maybe Path
largestFile = foldl f Nothing (onlyFiles root)
 where
    f :: Maybe Path -> Path -> Maybe Path
    f Nothing b = pure b
    f a b = case compare (size b) (a >>= size) of
      GT -> pure b
      _  -> a

smallestFile :: Maybe Path
smallestFile = foldl f Nothing (onlyFiles root)
  where
    f :: Maybe Path -> Path -> Maybe Path
    f Nothing b = pure b
    f a b = case compare (size b) (a >>= size) of
      LT -> pure b
      _  -> a

-- this is a weird exercise, the directory is literally in the path. It would
-- make more sense to return an array of paths, but I don't want to figure out
-- parsing code in Purescript yet.






