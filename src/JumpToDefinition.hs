{-# LANGUAGE OverloadedStrings #-}

module JumpToDefinition (definitionRequestToResponse) where

import Data.Int
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Language.Haskell.LSP.Types.Lens as LSPLens
import Language.Haskell.LSP.Types as L
import Language.Haskell.LSP.Types (Uri, uriToFilePath, getUri, _line, _character )
import Data.Text as T
import Data.List as List
import qualified Data.ByteString.Char8 as BS
import Data.ProtoLens (decodeMessage)
import System.FilePath.Find as Find
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix (makeRelative)

import Proto.Semanticdb as S
import Proto.Semanticdb_Fields (documents, occurrences, role, symbol, startLine, endLine, startCharacter, endCharacter, range, uri)
import LSP
import Helpers

--TODO: use Monad transformer

definitionRequestToResponse :: DefinitionRequest -> IO DefinitionResponse
definitionRequestToResponse definitionRequest = do
  maybeLocation <- findLocationFromRequest definitionRequest
  return $ definitionResponse definitionRequest maybeLocation

findLocationFromRequest :: DefinitionRequest -> IO (Maybe L.Location)
findLocationFromRequest definitionRequest = do
  let pos = definitionRequest^.LSPLens.params^.LSPLens.position
  let uri = definitionRequest^.LSPLens.params^.LSPLens.textDocument^.LSPLens.uri
  -- find the `TextDocument` of the request
  maybeTextDocument <- textDocumentWthUri uri
  case maybeTextDocument of
    Nothing -> return $ Nothing
    -- find the symbol in the `TextDocument` at the given `Position`
    Just textDocument -> case occurrenceAtPosition pos textDocument of
      Nothing -> return $ Nothing
      Just symbolOccurence ->
        case symbolOccurence^.role of
          S.SymbolOccurrence'UNKNOWN_ROLE -> return Nothing
          -- the symbol is already the definition itself
          S.SymbolOccurrence'DEFINITION -> return $ Just $ lspLocation uri symbolOccurence
          -- the symbol is a reference, we search first if it's defined in the same file
          S.SymbolOccurrence'REFERENCE ->  case definitionInTexDocument symbolOccurence textDocument of
            -- yes, we can directly return the location
            Just definitionSymbol -> return $ Just $ lspLocation uri definitionSymbol
            Nothing -> do
              -- No, we'll try optimistic search, maybe the file has the same name as the symbol
              maybeOptimistResult <- defnitionWithOptimisticSearch symbolOccurence
              case maybeOptimistResult of
                Just symbolWithTextDocument -> responseFromSymbolWithTextDocument symbolWithTextDocument
                Nothing -> do
                  -- No, we have to search in all project files for the definition.
                  maybeResult <- defnitionInProjectFiles symbolOccurence
                  case maybeResult of
                    Nothing -> return Nothing
                    Just symbolWithTextDocument -> responseFromSymbolWithTextDocument  symbolWithTextDocument


defnitionWithOptimisticSearch :: S.SymbolOccurrence -> IO(Maybe (S.SymbolOccurrence, S.TextDocument))
defnitionWithOptimisticSearch symbolOccurence = do
  findFile (extractFileName symbolOccurence) >>= searchOccurence symbolOccurence
  where
  findFile fileName = getCurrentDirectory >>= Find.find always (Find.fileName ==? fileName)
  extractFileName :: S.SymbolOccurrence -> FilePath
  extractFileName symbolOccurence =
    let
      takeName = List.takeWhile (\char -> not (char == '.' || char == '#'))
      dropPrefixPath = List.reverse . (List.takeWhile (\char -> not (char == '/' ))) . List.reverse
    in
      (takeName . dropPrefixPath $ T.unpack $ (symbolOccurence^.symbol)) ++ ".scala.semanticdb"


defnitionInProjectFiles :: S.SymbolOccurrence -> IO(Maybe (S.SymbolOccurrence, S.TextDocument))
defnitionInProjectFiles symbolOccurence = do
  listAllfiles >>= searchOccurence symbolOccurence
