{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Dirs (projcats, showcat, findProj, classify, Classification (DNE, File, Dir)) where

import Data.Bool (bool)
import Control.Spoon (spoon)
import Data.Maybe (fromMaybe)
import System.Directory (doesDirectoryExist, doesPathExist, getHomeDirectory, getUserDocumentsDirectory, listDirectory, doesFileExist)
import System.Environment (getEnv)

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

findProj :: IO (Maybe FilePath)
findProj = do
    home <- getHomeDirectory
    doc <- getUserDocumentsDirectory
    he <- doesDirectoryExist (home ++ "/proj")
    de <- doesDirectoryExist (doc ++ "/proj")
    if he
        then return (return (home ++ "/proj"))
        else
            if de
                then return (return (doc ++ "/proj"))
                else Nothing <$ putStrLn ("Could not find the proj directory in " ++ home ++ " or " ++ doc)

type ProjPath = String

tryContents :: FilePath -> IO (Maybe [FilePath])
tryContents p =
    filter ((/= Just '.') . head') <$$> do
        exists <- doesPathExist p
        if exists
            then do
                isdir <- doesDirectoryExist p
                if isdir
                    then do
                        Just <$> listDirectory p
                    else Nothing <$ putStrLn ("Not a directory: " ++ p)
            else Nothing <$ putStrLn ("Does not exist: " ++ p)

projcats :: ProjPath -> IO (Maybe [FilePath])
projcats = tryContents

entries :: ProjPath -> FilePath -> IO (Maybe [FilePath])
entries proj p = tryContents (proj ++ "/" ++ p)

columns :: IO Int
columns = do
    c <- getEnv "COLUMNS"
    case spoon (read c) of
        Nothing -> 80 <$ putStrLn "The $COLUMNS variable was not set. Assuming a width of 80 characters."
        Just x -> return x

splitUntil :: ([a] -> Bool) -> [a] -> ([a], [a])
splitUntil !p !l = last $ takeWhile (p . fst) $ map (`splitAt` l) [0 .. 1 + length l]

strip :: String -> String
strip [] = []
strip (' ' : (strip -> [])) = []
strip (!x : !xs) = x : strip xs

mapLast :: (a -> a) -> [a] -> [a]
mapLast f [] = []
mapLast f [!x] = [f x]
mapLast f (!x : !xs) = x : mapLast f xs

writeLines :: String -> Int -> String -> [String] -> [IO String]
writeLines _ _ _ [] = []
writeLines location width prefix lst =
    let
        shortEnough = (<= width) . length . (prefix ++) . concat . mapLast strip
        (line, rest) = splitUntil shortEnough lst
     in
        ((("\ESC[38;5;12;1m" ++ prefix) ++) . concat <$> sequence (mapLast (fmap strip) $ map (colorate location) line))
            : writeLines location width (' ' <$ prefix) rest

colorate :: String -> String -> IO String
colorate c e = do
    home <- getHomeDirectory
    let fp = home ++ "/proj/" ++ c ++ "/" ++ strip e
    isd <- doesDirectoryExist fp
    if isd then pure ("\ESC[38;5;2m" ++ e) else pure ("\ESC[38;5;3m" ++ e)

splitEvery :: Int -> [a] -> [[a]]
splitEvery n l
    | length l <= n = [l]
    | otherwise = let (h, t) = splitAt n l in h : splitEvery n t

showcat :: ProjPath -> String -> IO ()
showcat proj c = do
    let cl = max 5 $ 20 - length c
    let c' = c ++ replicate cl ' '
    cols <- columns
    let width = cols - 5
    (fromMaybe [] -> entrs) <- entries proj c
    let ml = 5 + maximum (length <$> entrs)
    let rowlength = div (width - cl) ml
    let rl' = div (length entrs) (1 + div (length entrs) rowlength)
    formatted <- mapM (\s -> (++ replicate (ml - length s) ' ') <$> colorate c s) entrs
    let rows = splitEvery rl' formatted
    case rows of
        [] -> putStrLn $ "\ESC[38;5;12;1m" ++ c'
        row : rrows -> do
            putStrLn $ "\ESC[38;5;12;1m" ++ c' ++ concat (mapLast strip row)
            mapM_ (\r -> putStrLn $ (' ' <$ c') ++ concat (mapLast strip r)) rrows

data Classification = DNE | File | Dir

classify :: FilePath -> IO Classification
classify fp = doesPathExist fp >>= bool (return DNE) (
        doesFileExist fp >>= bool (
            doesDirectoryExist fp >>= bool (
                DNE <$ putStrLn (fp ++ " exists, but behaves strangely.")
            ) (return Dir)
        ) (return File)
    )
