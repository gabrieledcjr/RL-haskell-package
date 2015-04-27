import qualified Data.HashTable.IO as H
import qualified Data.Maybe as M

type HashTable k v = H.BasicHashTable k v


foo :: IO (HashTable String Float)
foo = do
        ht <- H.new
        H.insert ht "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 1
        b <- H.lookup ht "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        print b
        H.insert ht "aaa" 5
        b <- H.lookup ht "aa"
        (ht, a) <- checkIt b ht
        print a
        b <- H.toList ht
        print b
        return ht
        where checkIt x ht = case x of
                              Nothing -> do H.insert ht "aa" 0.0
                                            return (ht, 0.0)
                              Just x -> return (ht, x)

