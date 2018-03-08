{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Readers.VerboseJSON
    ( parseInline
    , parseBlock
    , parsePandoc
    , readVerboseJSON
    ) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.HashMap.Strict as H
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Error
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Text (Text)
import Data.Version (versionBranch)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.Aeson.BetterErrors as P
import qualified Data.Vector as V

type Parser a = P.Parse CustomError a

data CustomError =
    UnknownConstructor Text
    | UnknownType Text Text
    | VersionError String
    deriving (Eq, Show, Ord)

tup2_ :: Parser (String, String)
tup2_ = (,) <$> P.nth 0 P.asString <*> P.nth 1 P.asString

-- | Create a parser for a type constructor with two arguments
cons2_ :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
cons2_ f pa pb = f <$> P.nth 0 pa <*> P.nth 1 pb

-- | Create a parser for a type constructor with three arguments
cons3_ :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
cons3_ f pa pb pc = f <$> P.nth 0 pa <*> P.nth 1 pb <*> P.nth 2 pc

type_ :: Parser Text
type_ = P.key "t" P.asText

enum_ :: (Text -> Maybe a) -> Parser a
enum_ f = do
    t <- type_
    maybe (P.throwCustomError $ UnknownConstructor t) return (f t)

asCitationMode :: Parser CitationMode
asCitationMode = enum_ $ \t ->
    case t of
        "AuthorInText"   -> Just AuthorInText
        "SuppressAuthor" -> Just SuppressAuthor
        "NormalCitation" -> Just NormalCitation
        _                -> Nothing

asCitation :: Parser Citation
asCitation = do
    citationId'      <- P.key "citationId" P.asString
    citationPrefix'  <- P.key "citationPrefix" asInlines
    citationSuffix'  <- P.key "citationSuffix" asInlines
    citationMode'    <- P.key "citationMode" asCitationMode
    citationNoteNum' <- P.key "citationNoteNum" P.asIntegral
    citationHash'    <- P.key "citationHash" P.asIntegral
    return Citation { citationId = citationId'
                    , citationPrefix = citationPrefix'
                    , citationSuffix = citationSuffix'
                    , citationMode = citationMode'
                    , citationNoteNum = citationNoteNum'
                    , citationHash = citationHash'
                    }

asQuoteType :: Parser QuoteType
asQuoteType = enum_ $ \t ->
    case t of
        "SingleQuote" -> Just SingleQuote
        "DoubleQuote" -> Just DoubleQuote
        _             -> Nothing

mathType_ :: Parser MathType
mathType_ = enum_ $ \t ->
    case t of
      "DisplayMath" -> Just DisplayMath
      "InlineMath"  -> Just InlineMath
      _             -> Nothing

listNumberStyle_ :: Parser ListNumberStyle
listNumberStyle_ = enum_ $ \t ->
    case t of
        "DefaultStyle" -> Just DefaultStyle
        "Example"      -> Just Example
        "Decimal"      -> Just Decimal
        "LowerRoman"   -> Just LowerRoman
        "UpperRoman"   -> Just UpperRoman
        "LowerAlpha"   -> Just LowerAlpha
        "UpperAlpha"   -> Just UpperAlpha
        _              -> Nothing

listNumberDelim_ :: Parser ListNumberDelim
listNumberDelim_ = enum_ $ \t ->
    case t of
      "DefaultDelim" -> Just DefaultDelim
      "Period"       -> Just Period
      "OneParen"     -> Just OneParen
      "TwoParens"    -> Just TwoParens
      _              -> Nothing

asAlignment :: Parser Alignment
asAlignment = enum_ $ \t ->
    case t of
      "AlignLeft"    -> Just AlignLeft
      "AlignRight"   -> Just AlignRight
      "AlignCenter"  -> Just AlignCenter
      "AlignDefault" -> Just AlignDefault
      _              -> Nothing

asTarget :: Parser Target
asTarget = tup2_

asInlineQuoted :: Parser Inline
asInlineQuoted = Quoted <$> P.nth 0 asQuoteType <*> P.nth 1 asInlines

asAttr :: Parser Attr
asAttr = (,,)
    <$> P.nth 0 P.asString
    <*> P.nth 1 (P.eachInArray P.asString)
    <*> P.nth 2 (P.eachInArray tup2_)

asInlineCode :: Parser Inline
asInlineCode = Code <$> P.nth 0 asAttr <*> P.nth 1 P.asString

asFormat :: Parser Format
asFormat = Format <$> P.asString

asInline :: Parser Inline
asInline = do
    t <- type_
    let content = P.key "c"
        contentAsInlines = content asInlines
    case t of
        "Str"         -> Str <$> content P.asString
        "Emph"        -> Emph <$> contentAsInlines
        "Strong"      -> Strong <$> contentAsInlines
        "Strikeout"   -> Strikeout <$> contentAsInlines
        "Superscript" -> Superscript <$> contentAsInlines
        "Subscript"   -> Subscript <$> contentAsInlines
        "SmallCaps"   -> SmallCaps <$> contentAsInlines
        "Quoted"      -> content asInlineQuoted
        "Cite"        -> content $ cons2_ Cite (P.eachInArray asCitation) asInlines
        "Code"        -> content asInlineCode
        "Space"       -> return Space
        "SoftBreak"   -> return SoftBreak
        "LineBreak"   -> return LineBreak
        "Math"        -> content $ cons2_ Math mathType_ P.asString
        "RawInline"   -> content $ cons2_ RawInline asFormat P.asString
        "Link"        -> content $ cons3_ Link asAttr asInlines asTarget
        "Image"       -> content $ cons3_ Image asAttr asInlines asTarget
        "Note"        -> Note <$> content asBlocks
        "Span"        -> content $ cons2_ Span asAttr asInlines
        _             -> P.throwCustomError $ UnknownType "Inline" t

asInlines :: Parser [Inline]
asInlines = P.eachInArray asInline

asDefinitionEntry :: Parser ([Inline], [[Block]])
asDefinitionEntry = cons2_ (,) asInlines (P.eachInArray asBlocks)

asListAttributes :: Parser ListAttributes
asListAttributes = cons3_ (,,) P.asIntegral listNumberStyle_ listNumberDelim_

asBlock :: Parser Block
asBlock = do
    t <- type_
    let content = P.key "c"
        contentAsInlines = content asInlines
    case t of
        "Plain"          -> Plain <$> contentAsInlines
        "Para"           -> Para <$> contentAsInlines
        "LineBlock"      -> LineBlock <$> content (P.eachInArray asInlines)
        "CodeBlock"      -> content $ cons2_ CodeBlock asAttr P.asString
        "RawBlock"       -> content $ cons2_ RawBlock asFormat P.asString
        "BlockQuote"     -> BlockQuote <$> content asBlocks
        "OrderedList"
            -> content $ cons2_ OrderedList asListAttributes (P.eachInArray asBlocks)
        "BulletList"     -> BulletList <$> content (P.eachInArray asBlocks)
        "DefinitionList"
            -> DefinitionList <$> content (P.eachInArray asDefinitionEntry)
        "Header"         -> content $ cons3_ Header P.asIntegral asAttr asInlines
        "HorizontalRule" -> return HorizontalRule
        "Table"          -> content asPandocTable
        "Div"            -> content $ cons2_ Div asAttr asBlocks
        "Null"           -> return Null
        _                -> P.throwCustomError $ UnknownType "Block" t

asBlocks :: Parser [Block]
asBlocks = P.eachInArray asBlock

asTableCell :: Parser TableCell
asTableCell = asBlocks

asPandocTable :: Parser Block
asPandocTable = do
    caption <- P.nth 0 asInlines
    align   <- P.nth 1 (P.eachInArray asAlignment)
    width   <- P.nth 2 (P.eachInArray P.asRealFloat)
    header  <- P.nth 3 (P.eachInArray asTableCell)
    rows    <- P.nth 4 (P.eachInArray (P.eachInArray asTableCell))
    return $ Table caption align width header rows

asMetaMap :: Parser (M.Map String MetaValue)
asMetaMap = mkMap <$> P.eachInObject asMetaValue
    where mkMap l = M.fromList [(T.unpack k, v) | (k, v) <- l]

asMetaValue :: Parser MetaValue
asMetaValue = do
    t <- type_
    let content = P.key "c"
    case t of
        "MetaMap"     -> MetaMap     <$> content asMetaMap
        "MetaList"    -> MetaList    <$> content (P.eachInArray asMetaValue)
        "MetaBool"    -> MetaBool    <$> content P.asBool
        "MetaString"  -> MetaString  <$> content P.asString
        "MetaInlines" -> MetaInlines <$> content asInlines
        "MetaBlocks"  -> MetaBlocks  <$> content asBlocks
        _             -> P.throwCustomError $ UnknownType "MetaValue" t

checkPandocVersion :: Parser ()
checkPandocVersion = do
    version <- P.key "pandoc-api-version" (P.eachInArray P.asIntegral)
    validate version
    where
        curMajor : curMinor : _ = versionBranch pandocTypesVersion

        validate version =
            case version of
                major : minor : _ ->
                    when (major /= curMajor || minor /= curMinor) $
                        P.throwCustomError $ VersionError $ concat
                            ["pandoc version mismatch JSON generated for "
                            , show curMajor, ".", show curMinor, ", but current"
                            , "pandoc-types version is "
                            , show major, ".", show minor]
                _ -> P.throwCustomError $ VersionError $ concat
                    [ "wrong format for version field: must have at least"
                    , " major.minor specified"]

asPandoc :: Parser Pandoc
asPandoc = do
    checkPandocVersion
    Pandoc
        <$> (Meta <$> P.key "meta" asMetaMap)
        <*> P.key "blocks" asBlocks

parseInline :: JSON.Value -> JSON.Parser Inline
parseInline = P.toAesonParser showError asInline
    where showError = T.pack .  show

parseBlock :: JSON.Value -> JSON.Parser Block
parseBlock = P.toAesonParser showError asBlock
    where showError = T.pack .  show

parsePandoc :: JSON.Value -> JSON.Parser Pandoc
parsePandoc = P.toAesonParser showError asPandoc
    where showError = T.pack .  show

ppError :: P.ParseError CustomError -> String
ppError err = T.unpack . T.unlines $ P.displayError (T.pack . show) err

pruneJSON :: Int -> JSON.Value -> JSON.Value
pruneJSON 0 (JSON.Object _) = JSON.Object $ H.empty
pruneJSON 0 (JSON.Array _) = JSON.Array $ V.empty
pruneJSON 0 x = x
pruneJSON n (JSON.Object om) = JSON.Object $ fmap (pruneJSON (n - 1)) om
pruneJSON n (JSON.Array v) = JSON.Array $ fmap (pruneJSON (n - 1)) v
pruneJSON _ x = x

-- | Extract a given JSON subtree
filterJSON :: [P.PathPiece] -> JSON.Value -> Maybe JSON.Value
filterJSON [] v = return v
filterJSON (P.ObjectKey k:xs) v =
    case v of
        JSON.Object om -> do
            res <- H.lookup k om
            let genMap x = H.insert k x $ fmap (pruneJSON 1) om
            JSON.Object . genMap <$> (filterJSON xs res)
        _ -> Nothing
filterJSON (P.ArrayIndex idx:xs) v =
    case v of
        JSON.Array arr -> do
            res <- arr V.!? idx
            (JSON.Array . V.singleton) <$> filterJSON xs res
        _ -> Nothing

reportError
    :: JSON.Value
    -> P.ParseError CustomError
    -> String
reportError value err =
    case err of
        P.InvalidJSON m -> concat [
            "Low-level basic JSON parsing failed (aeson message :",
            m, ")"]
        P.BadSchema path _ ->
            let x = maybe (error "failed during error generation") id $ (filterJSON path value) in
                unlines [ppError err, UTF8.toStringLazy (JSON.encodePretty x)]

-- | Parse a JSON pandoc AST, with verbose error messages !
readVerboseJSON
    :: PandocMonad m
    => ReaderOptions -- ^ Reader options
    -> T.Text        -- ^ Content to parse (assuming @'\n'@ line endings)
    -> m Pandoc
readVerboseJSON _ content = do
    v <- either (throwError . aesonParseError) return parseValue
    let parseResult = P.parseValue asPandoc v
    either (throwError . PandocParseError . reportError v) return parseResult
    where
        aesonParseError err =
            PandocParseError $ concat
                [ "Low-level basic JSON parsing failed (aeson message :"
                , err, ")" ]

        parseValue :: Either String JSON.Value
        parseValue =
            JSON.eitherDecode' . BL.fromStrict . UTF8.fromText $ content
