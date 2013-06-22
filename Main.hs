{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Control.Monad
import Data.List
import qualified Data.Map as M
import System.IO

infixr 3 .&&
f .&& g = (f &&& g) >>^ snd

addAttrTag vocab (attr, tag) =
  case M.lookup tag vocab of
    Just _  -> M.adjust (attr :) tag vocab
    Nothing -> M.insert tag [attr] vocab

changeKey k1 k2 m =
  case M.lookup k1 m of
    Just v  -> M.delete k1 $ M.insert k2 v m
    Nothing -> m

main = do
  doc <- fromUrl "http://www.w3.org/html/wg/drafts/html/master/index.html"
  res <- runX $ do
    doc //> css "table" /> (
      ( css "thead tr"
        >>> (css "th" /> hasText (isInfixOf "Attribute"))
        &&& (css "th" /> hasText (isInfixOf "Element(s)"))
      ) .&& (
        css "tbody tr"
        >>> (css "th:first-child" //> css "code" /> getText)
        &&& (css "td:first-child" //> css "a"    /> getText)
      )
     )
  let vocab = changeKey "HTML elements" "*"
            $ foldl' addAttrTag M.empty res
  withFile "w3c-html-vocab.txt" WriteMode $ \h -> do
    forM_ (M.toList vocab) $ \(tag, attrs) -> do
      hPutStrLn h $ unwords (tag : attrs)
