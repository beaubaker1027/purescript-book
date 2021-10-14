module Test.MySolutions where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Parallel (parTraverse, parallel, sequential)
import Data.Array as ARR
import Data.Either (Either(..))
import Data.Foldable (foldl, oneOf)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, length)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff, attempt, delay)
import Effect.Class.Console (log)
import Effect.Exception (message, Error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath, dirname)

-- Note to reader: Add your solutions to this file
-- Why doesnt readTextFile and writeTextFile not need attempt per the solutions file?
readFile :: FilePath -> Aff ( Either Error String )
readFile p = do
    attempt $ readTextFile UTF8 p

readManyFiles :: Array FilePath -> Aff ( Either Error (Array String) )
readManyFiles xs = do
    attempt $ traverse (readTextFile UTF8) xs

readManyFilesPar :: Array FilePath -> Aff ( Either Error (Array String) )
readManyFilesPar xs = do
    attempt $ parTraverse (readTextFile UTF8) xs

mergeTexts :: Array String -> String
mergeTexts = foldl (<>) ""

concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles x y z = concatenateMany [x,y] z

concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany xs o = do
    manyTexts <- readManyFiles xs
    case manyTexts of
        Left err -> log $ message err
        Right texts -> do
            writeStatus <- attempt $ writeTextFile UTF8 o (mergeTexts texts)
            case writeStatus of
                Left e  -> log $ message e
                Right u -> pure u

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters p = do
    text <- readFile p
    pure case text of
        Left e  -> Left e
        Right t -> Right $ length t   

writeGet :: String -> FilePath -> Aff Unit
writeGet url o = do 
    result <- getUrl url
    writeStatus <- attempt $ writeTextFile UTF8 o result
    case writeStatus of
        Left e  -> log $ message e
        Right u -> pure u

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel xs o = do
    manyTexts <- readManyFilesPar xs
    case manyTexts of
        Left e -> log $ message e
        Right texts -> do 
            writeStatus <- attempt $ writeTextFile UTF8 o (mergeTexts texts)
            case writeStatus of
                Left e  -> log $ message e
                Right u -> pure u

getWithTimeout :: Number -> String -> Aff ( Maybe String )
getWithTimeout d url = do
    sequential $ oneOf 
        [ parallel $ Just <$> getUrl url
        , parallel $ Nothing <$ delay ( Milliseconds d ) ]

recurseFiles :: FilePath -> Aff (Array FilePath)
recurseFiles root = do
    file <- attempt $ readTextFile UTF8 root
    case file of
        Left  _     -> pure []
        Right text  -> do
            let nestedFiles = split (Pattern "\n") text
            case ARR.null nestedFiles of
                true    -> do pure [root]
                false   -> do
                    children <- parTraverse recurseFiles (((<>) ((dirname root) <> "/") ) <$> nestedFiles)
                    let
                        flatChildren = ARR.concat children
                    pure $ ARR.cons root flatChildren



-- Samples
copyFile :: FilePath -> FilePath -> Aff Unit
copyFile file1 file2 = do
  my_data <- readTextFile UTF8 file1
  writeTextFile UTF8 file2 my_data

getUrl :: String -> Aff String
getUrl url = do
  result <- AX.get ResponseFormat.string url
  pure case result of
    Left err -> "GET /api response failed to decode: " <> AX.printError err
    Right response -> response.body