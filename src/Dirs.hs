{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Dirs (projcats, showcat, findProj) where

import Control.Spoon (spoon)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    getHomeDirectory,
    getUserDocumentsDirectory,
    listDirectory,
 )
import System.Environment (getEnv)

-- | head if it was total
head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

-- this might actually be the funniest function i've ever written

-- | double fmap
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

{- when i was writing this, i couldn't remember where i put the proj directory, and didn't care enough to check.
i was on my laptop, i'm not really sure why i didn't check.
i think maybe the directory is in Documents on my desktop?
it's been out of order for a while, so i have no idea.
anyway, on my primary computer atm (laptop), it's actually in Desktop.
so i put a symlink in $HOME and it works. -}

-- | tries to get the path to proj
findProj :: IO (Maybe FilePath)
findProj = do
    home <- getHomeDirectory
    doc <- getUserDocumentsDirectory
    he <- doesDirectoryExist (home ++ "/proj") -- does home have a proj?
    de <- doesDirectoryExist (doc ++ "/proj") -- analogous for Documents
    if he
        then return (return (home ++ "/proj"))
        else
            if de
                then return (return (doc ++ "/proj"))
                else Nothing <$ putStrLn ("Could not find the proj directory in " ++ home ++ " or " ++ doc)

{- clarify type signatures
FilePath is the same way
-}
type ProjPath = String

-- i could've done Either String [FilePath], to let the caller handle the errors, but i'm using IO anyway.
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

-- why is this a separate function?
projcats :: ProjPath -> IO (Maybe [FilePath])
projcats = tryContents

-- | given a category in proj, tries to return a list of its entries.
entries :: ProjPath -> FilePath -> IO (Maybe [FilePath])
entries proj p = tryContents (proj ++ "/" ++ p)

-- lmao
columns :: IO Int
columns = do
    c <- getEnv "COLUMNS"
    case spoon (read c) of
        Nothing -> do
            putStrLn "The $COLUMNS variable was not set to an integer."
            putStrLn "Assuming a width of 80 characters."
            return 80
        Just x -> return x

-- | sort of a mix of takeWhile and dropWhile, but not quite.
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

-- | given a category and an entry, adds ANSI escapes to color the entry.
colorate :: String -> String -> IO String
colorate c e = do
    home <- getHomeDirectory
    let fp = home ++ "/proj/" ++ c ++ "/" ++ strip e
    isd <- doesDirectoryExist fp
    return $ if isd then "\ESC[38;5;2m" ++ e else "\ESC[38;5;3m" ++ e

-- | Splits a list into sublists of specified length.
splitEvery :: Int -> [a] -> [[a]]
splitEvery n l
    | length l <= n = [l]
    | otherwise = let (h, t) = splitAt n l in h : splitEvery n t

showcat :: ProjPath -> String -> IO ()
showcat proj c = do
    let cl = max 5 $ 20 - length c -- how many spaces to put after the category (min. 5, ensures the category takes up 20 characters)
    let c' = c ++ replicate cl ' ' -- add the spaces
    cols <- columns -- how wide is the terminal?
    let width = cols - 5 -- i think i wanted to make sure i wasn't running up into the edge?
    (fromMaybe [] -> entrs) <- entries proj c -- get the entries to be shown
    let ml = 5 + maximum (length <$> entrs) -- gives all; entries the same amount of space, and min. 5 spaces of padding
    let rowlength = div (width - cl) ml -- how many entries can fit in a line
    let rl' = div (length entrs) (1 + div (length entrs) rowlength) -- ??????????
    formatted <- mapM (\s -> (++ replicate (ml - length s) ' ') <$> colorate c s) entrs -- colorate & pad
    let rows = splitEvery rl' formatted -- split into rows
    case rows of
        [] -> putStrLn $ "\ESC[38;5;12;1m" ++ c'
        row : rrows -> do
            putStrLn $ "\ESC[38;5;12;1m" ++ c' ++ concat (mapLast strip row) -- only put the category on the first line
            mapM_ (\r -> putStrLn $ (' ' <$ c') ++ concat (mapLast strip r)) rrows
