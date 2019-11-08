{-# LANGUAGE OverloadedStrings #-}

module FindReferences (referenceRequestToResponse)  where
import Data.Traversable (traverse, sequence)
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


referenceRequestToResponse ::  ReferencesRequest -> IO ReferencesResponse
referenceRequestToResponse referenceRequest = do
  locations <- findLocationsFromRequest referenceRequest
  return $ referencesResponse referenceRequest locations

findLocationsFromRequest :: ReferencesRequest -> IO ([L.Location])
findLocationsFromRequest referenceRequest = do
  let pos = referenceRequest^.LSPLens.params^.LSPLens.position
  let uri = referenceRequest^.LSPLens.params^.LSPLens.textDocument^.LSPLens.uri
  -- find the `TextDocument` of the request
  maybeTextDocument <- textDocumentWthUri uri
  case maybeTextDocument of
    Nothing -> return $ []
    -- find the symbol in the `TextDocument` at the given `Position`
    Just textDocument -> case occurrenceAtPosition pos textDocument of
      Nothing -> return $ []
      Just symbolOccurence ->
        case symbolOccurence^.role of
          S.SymbolOccurrence'UNKNOWN_ROLE -> return []
          _ -> do
            allSymbols <- allInProjectFiles symbolOccurence
            traverse resFromSymbolWithTextDocument allSymbols


allInProjectFiles :: S.SymbolOccurrence -> IO([(S.SymbolOccurrence, S.TextDocument)])
allInProjectFiles symbolOccurence = do
  textDocuments <- listAllfiles >>= traverse  listTextDocumentFromFilePath
  return $ List.concat textDocuments >>=  sameSymbolsInTexDocument symbolOccurence
