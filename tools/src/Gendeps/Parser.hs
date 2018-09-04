{-# LANGUAGE NoImplicitPrelude #-}

module Gendeps.Parser
       ( digraphRemoval
       , getDeps
       , mainPackage
       , mainPackages
       , rankRemoval
       , streamDeps
       , styleRemoval
       ) where

import           Universum hiding (many)

import           Conduit (decodeUtf8C, encodeUtf8C, mapC, runConduitRes,
                     sourceFile, stdoutC, (.|))
import           Data.List (concat)
import qualified Data.Text as T
import           System.Process (callCommand)
import           Text.Parsec

mainPackages :: Parsec String () [String]
mainPackages = do
    _ <- digraphRemoval
    packages <- many (mainPackage <* styleRemoval)
    return $ map (intercalate "-") packages

getDeps :: IO ()
getDeps = do
    let createDot = callCommand "stack dot --external > tools/src/Gendeps/gendeps.dot"
    createDot `finally` streamDeps

streamDeps :: IO ()
streamDeps =
    runConduitRes
     $ sourceFile "tools/src/Gendeps/gendeps.dot"
    .| decodeUtf8C
    .| mapC T.unpack
    .| mapC ((parse mainPackages) "")
    .| mapC show
    .| mapC T.pack
    .| encodeUtf8C
    .| stdoutC

------------------------------Helper Parsers-----------------------------------

digraphRemoval :: Parsec String () [String]
digraphRemoval =
    concat <$> manyTill (many alphaNum `sepBy` space) (char '{') <* many space

mainPackage :: Parsec String () [String]
mainPackage =
    between
        (char '"')
        (char '"')
        (many alphaNum `sepBy` char ('-')) <* many space

styleRemoval :: Parsec String () ()
styleRemoval = do
    _ <- between
             (char '[')
             (char ']')
             (many alphaNum >> char '=' >> many alphaNum)
    _ <- char ';'
    _ <- many space
    pure ()

rankRemoval :: Parsec String () ()
rankRemoval = do
    _ <- between (char '{')
                 (char '}')
                 (string "rank=max;"
                     *> space
                     *> stringParse
                     *> char ';'
                     *> space)
    pure ()

stringParse :: Parsec String () String
stringParse = between
             (char '"')
             (char '"')
             (many alphaNum)
