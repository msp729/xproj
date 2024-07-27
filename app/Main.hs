{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Dirs (findProj, projcats, showcat)
import System.Directory (makeAbsolute)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

fillIn :: Maybe [a] -> [a]
fillIn = fromMaybe []

putLn :: IO ()
putLn = putStrLn ""

main :: IO ()
main = do
    p <- findProj >>= maybe (return Nothing) (fmap Just . makeAbsolute)
    case p of
        Nothing -> putStrLn "no proj!"
        Just proj -> do
            hSetBuffering stdout NoBuffering
            (fillIn -> cats) <- projcats proj
            listmain proj cats

listmain :: String -> [String] -> IO ()
listmain proj cats = do
    let acts = showcat proj <$> cats
    let acts' = intersperse putLn acts
    sequence_ acts'
