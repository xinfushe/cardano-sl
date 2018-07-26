module Util.Conduit
    ( parseP
    , fold'
    ) where

import           Universum hiding (foldl)

import           Control.Foldl (Fold (..))
import           Data.Attoparsec.ByteString (IResult (..), Parser, Result,
                     parse)
import           Data.Conduit (ConduitT, await, runConduit, yield, (.|))
import           Data.Conduit.Combinators (foldl)

parseP :: forall m a. Monad m => Parser a -> ConduitT ByteString a m ()
parseP p = go (parse p)
  where
    go :: (ByteString -> Result a) -> ConduitT ByteString a m ()
    go p' = whenJustM await (consume p')

    consume :: (ByteString -> Result a) -> ByteString -> ConduitT ByteString a m ()
    consume p' chunk = case p' chunk of
        Fail _ _ e    -> error $ toText e
        Done chunk' a -> yield a >> consume (parse p) chunk'
        Partial p''   -> go p''

fold' :: Monad m => Fold a b -> ConduitT () a m () -> m b
fold' (Fold step begin extract) ct = extract <$> runConduit (ct .| foldl step begin)
