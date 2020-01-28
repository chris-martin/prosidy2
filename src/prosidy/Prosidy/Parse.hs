{- |
Module      : Prosidy.Parse
Description : Parse raw text into Prosidy documents
Copyright   : (c) James Alexander Feldman-Crough, 2019
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}

module Prosidy.Parse
    ( -- * Parsing Prosidy types from 'Data.Text.Text'
      parseDocument
    , parseDocumentMetadata
      -- * Reading & parsing Prosidy files
    , readDocument
    , readDocumentMetadata
      -- * Errors
    , Failure(..)
    , prettyFailure
    )
where

import           Prosidy.Types
import           Prosidy.Source
import           Prosidy.Internal.Optics

import           Text.Megaparsec         hiding ( token )
import           Text.Megaparsec.Char           ( char
                                                , string
                                                )

import qualified Data.Char                     as Char
import qualified Data.Text.Lazy                as Text.Lazy
import qualified Data.Map.Strict               as Map
import qualified Data.Sequence                 as Seq
import qualified Data.List                     as List
import qualified Data.Set                      as Set
import qualified Data.Text.Encoding            as Text.Encoding
import qualified Text.Megaparsec.Char          as Megaparsec
import qualified Data.ByteString               as ByteString

import           Control.Applicative            ( Alternative )
import           Data.Bifunctor                 ( first )
import           Text.Megaparsec.Char.Lexer     ( hexadecimal )
import           Data.Functor                   ( ($>) )
import           Data.Foldable                  ( fold
                                                , traverse_
                                                )
import           Control.Monad                  ( MonadPlus
                                                , void
                                                )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Control.Exception              ( Exception
                                                , throwIO
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT(..) )

{-
    If you run into errors, use the following combinator to get Megaparsec to 
    print out its state.

    > import qualified Text.Megaparsec.Debug
    > 
    > dbg :: Show a => String -> P a -> P a
    > dbg txt (P (ReaderT r)) = P . ReaderT $ \src ->
    >     Text.Megaparsec.Debug.dbg txt $ r src
-}

-------------------------------------------------------------------------------
-- | Parses a Prosidy 'Document' from its source.
--
-- The 'FilePath' parameter is only used for error reporting.
parseDocument :: FilePath -> Text -> Either Failure Document
parseDocument path = runP doc . makeSource path

-- | Reads a Prosidy 'Document' from the given 'FilePath'.
--
-- Errors will be thrown as exceptions. Use 'parseDocument' for a pure
-- implementation.
readDocument :: FilePath -> IO Document
readDocument filepath = do
    bytes <- ByteString.readFile filepath
    either throwIO pure . parseDocument filepath $ Text.Encoding.decodeUtf8With
        (\_ _ -> Just '\65533')
        bytes

-------------------------------------------------------------------------------
-- | Parses a Prosidy document's header 'Metadata' from source, stopping when the
-- header ends.
--
-- The 'FilePath' parameter is only used for error reporting.
parseDocumentMetadata :: FilePath -> Text -> Either Failure Metadata
parseDocumentMetadata path = runP docMetadata . makeSource path

-- | Reads a Prosidy document's 'Metadata' header from the given 'FilePath'.
--
-- Errors will be thrown as exceptions. Use 'parseDocumentMetadata' for a pure
-- implementation.
readDocumentMetadata :: FilePath -> IO Metadata
readDocumentMetadata filepath = do
    bytes <- ByteString.readFile filepath
    either throwIO pure
        . parseDocumentMetadata filepath
        $ Text.Encoding.decodeUtf8 bytes

-------------------------------------------------------------------------------
-- | A parsing error.
--
newtype Failure = Failure (ParseErrorBundle Text Void)
  deriving newtype (Exception, Show)

-- | Pretty-print a 'Failure' into a message acceptable for displaying to
-- users.
prettyFailure :: Failure -> String
prettyFailure (Failure e) = errorBundlePretty e

-------------------------------------------------------------------------------
newtype P a = P (ReaderT Source (Parsec Void Text) a)
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadFail, MonadPlus, MonadParsec Void Text)

type MetadataItem = (Key, Maybe Text)

runP :: P a -> Source -> Either Failure a
runP (P (ReaderT r)) src =
    first Failure $ parse (r src) (view sourcePath src) (view sourceText src)

-------------------------------------------------------------------------------
doc :: P Document
doc = do
    header <- docMetadata
    body   <- Series . Seq.fromList <$> many block
    eof
    pure $ Document header body

-------------------------------------------------------------------------------
docMetadata :: P Metadata
docMetadata = do
    -- skip past any blank lines at the start of the document
    skipSpaces *> skipMany endOfLine
    -- read each metadata item as a line-wide token
    items <- many docMetadataItem
    -- stop when we hit three dashes, alone, on a line
    docMetadataEnd
    pure $ foldMap itemToMetadata items

docMetadataEnd :: P ()
docMetadataEnd = do
    void $ string "---"
    try $ do
        skipSpaces
        newlineOrEOF
        skipSpaces
    skipMany endOfLine

docMetadataItem :: P MetadataItem
docMetadataItem = do
    itemKey <- key
    itemVal <- optional $ do
        metaItemSep
        option "" text <* skipSpaces
    endOfLines
    pure (itemKey, itemVal)

-------------------------------------------------------------------------------
block :: P Block
block = choice
    [ BlockTag <$> blockTag
    , BlockLiteral <$> literalTag
    , BlockParagraph <$> paragraph
    ]

blockTag :: P BlockTag
blockTag = do
    t <- genericTag (void $ string "#-") blockTagContents
    emptyLines
    pure t

blockTagContents :: P (Series Block)
blockTagContents = choice [ifBraces, ifBlock, ifNothing]
  where
    ifBraces = annotateSource $ fmap
        (foldMap $ \x src ->
            Series . Seq.singleton . BlockParagraph $ Paragraph x src
        )
        (token tagParagraph)
    ifBlock = Series . Seq.fromList <$> withBlockDelimiters
        (emptyLines *> many block)
    ifNothing = skipSpaces *> endOfLine $> mempty

literalTag :: P LiteralTag
literalTag = genericTag (void $ string "#=") $ do
    close <- blockTagDelim (void $ optional_ comment *> newlineOrEOF)
    literalBody close

literalBody :: P () -> P Literal
literalBody end = annotateSource $ do
    literalLines <- manyTill literalLine (try $ skipSpaces *> end)
    emptyLines
    pure
        $ Literal
        . Text.Lazy.toStrict
        . Text.Lazy.intercalate "\n"
        $ literalLines

literalLine :: P Text.Lazy.Text
literalLine = do
    line <- takeWhileP (Just "literal text") $ \ch -> ch /= '\r' && ch /= '\n'
    newlineOrEOF
    pure $ Text.Lazy.fromStrict line

blockTagDelim :: P () -> P (P ())
blockTagDelim slurp = do
    char ':'
    maybeLabel <- optional keyLike
    skipSpaces <* slurp
    pure $ do
        string "#:"
        traverse_ string maybeLabel
        skipSpaces

withBlockDelimiters :: P a -> P a
withBlockDelimiters parser = do
    close <- blockTagDelim endOfLine
    parser <* close

-------------------------------------------------------------------------------
inline :: P Inline
inline = choice [InlineTag <$> inlineTag, InlineFragment <$> fragment]

inlineTag :: P InlineTag
inlineTag = genericTag sigil . option mempty $ orEmpty tagParagraph
  where
    orEmpty = fmap $ maybe mempty getNonEmpty
    sigil   = try $ do
        void $ char '#'
        void . lookAhead $ satisfy isValidKeyHead

-------------------------------------------------------------------------------
paragraph :: P Paragraph
paragraph = annotateSource $ paragraphLike >>= maybe
    (fail "empty paragraph encountered")
    (pure . Paragraph)

paragraphLike :: P (Maybe (NonEmpty Series Inline))
paragraphLike = do
    ppLines <- paragraphLine `sepEndBy1` endOfLine
    emptyLines
    pure . nonEmpty . Series . Seq.fromList $ List.intercalate [Break] ppLines

paragraphLine :: P [Inline]
paragraphLine = do
    headItem <- inline
    tailItem <- many paragraphInline
    skipSpaces
    pure $ headItem : tailItem

paragraphInline :: P Inline
paragraphInline = (paragraphSpacer $> Break) <|> inline

paragraphSpacer :: P ()
paragraphSpacer = try $ do
    skipSpaces1
    notFollowedBy $ void (string "##") <|> newlineOrEOF

tagParagraph :: P (Maybe (NonEmpty Series Inline))
tagParagraph = between start end $ option Nothing paragraphLike
  where
    start = char '{' *> skipSpaces *> emptyLines
    end   = skipSpaces *> emptyLines *> char '}'

-------------------------------------------------------------------------------
genericTag :: P () -> P a -> P (Tagged a)
genericTag sigilParser bodyParser = annotateSource $ do
    sigilParser
    thisName     <- toKeyUnchecked <$> keyLike
    thisMetadata <- meta
    thisContent  <- bodyParser
    pure $ Tagged thisName thisMetadata thisContent

meta :: P Metadata
meta =
    option mempty
        $          between start end
        $          foldMap itemToMetadata
        <$>        metaItem
        `sepEndBy` metaSep
  where
    start = do
        char '['
        skipSpaces
        skipMany endOfLine
    end = char ']'

metaItem :: P MetadataItem
metaItem = do
    itemKey <- key <* emptyLines
    itemVal <- optional $ do
        metaItemSep <* emptyLines
        option "" quotedText
    skipSpaces <* emptyLines
    pure (itemKey, itemVal)

metaSep :: P ()
metaSep = do
    void $ char ','
    skipSpaces
    emptyLines

-------------------------------------------------------------------------------
escape :: P Char
escape = label "escape sequence" $ do
    void $ char '\\'
    choice
        [ oneOf @[] "#{}[]:='\"\\"
        , char 'n' $> '\n'
        , char 't' $> '\t'
        , char 'r' $> '\r'
        , char 'u' *> unicodeEscape
        ]

unicodeEscape :: P Char
unicodeEscape = Char.chr <$> hexadecimal

keyLike :: P Text
keyLike = do
    void . lookAhead $ satisfy isValidKeyHead
    takeWhile1P (Just "key") isValidKeyTail

key :: P Key
key = token $ toKeyUnchecked <$> keyLike

metaItemSep :: P ()
metaItemSep = token . void $ char ':' <|> char '='

quotedText :: P Text
quotedText = do
    delim <- char '\'' <|> char '"'
    parts <- many $ choice
        [ Text.Lazy.singleton <$> escape
        , Text.Lazy.fromStrict <$> takeWhile1P
            (Just "quoted text")
            (\ch -> ch /= delim && ch /= '\\')
        ]
    void $ char delim
    skipSpaces
    pure . Text.Lazy.toStrict . fold $ parts

fragment :: P Fragment
fragment = annotateSource (fmap Fragment text)

text :: P Text
text = do
    parts <- word `sepBy1` textSpace
    pure . Text.Lazy.toStrict . Text.Lazy.intercalate " " $ parts

textSpace :: P ()
textSpace = try $ do
    skipSpaces1
    notFollowedBy $ void (char '#') <|> newlineOrEOF

word :: P Text.Lazy.Text
word = fmap fold . some $ choice
    [ Text.Lazy.singleton <$> escape
    , Text.Lazy.fromStrict <$> takeWhile1P
        (Just "plain text")
        (\ch -> not $ Set.member ch reserved || Char.isSpace ch)
    ]
    where reserved = Set.fromList "#{}\\"

-------------------------------------------------------------------------------
comment :: P ()
comment = label "comment" $ do
    void $ string "##"
    void $ skipManyTill anySingle (lookAhead newlineOrEOF)

endOfLine :: P ()
endOfLine =
    -- This rule is a bit hairy! Specifically, there was a bug at the end of 
    -- a file that ended with a comment and no trailing newline.
    --
    -- Because we use `endOfLine` in repeat productions (many, some),
    -- endOfLine _has_ to consume input to prevent looping forever.
    -- In order to satisfy this:
    -- 
    -- 1. If its the end of a file, then we _must_ consume a comment.
    -- 2. If it's not the end of a file, then we _must_ consume at least 
    --    one newline.
            commentThenNewline <|> commentThenEOF
  where
    commentThenEOF     = comment <* eof
    commentThenNewline = try $ do
        optional_ comment
        void Megaparsec.newline
        skipSpaces

endOfLines :: P ()
endOfLines = skipSome endOfLine

emptyLines :: P ()
emptyLines = skipMany endOfLine

spaceChar :: P ()
spaceChar = do
    notFollowedBy newlineOrEOF
    void Megaparsec.spaceChar

skipSpaces :: P ()
skipSpaces = skipMany spaceChar

skipSpaces1 :: P ()
skipSpaces1 = skipSome spaceChar

token :: P a -> P a
token = (<* skipSpaces)

newlineOrEOF :: P ()
newlineOrEOF = void Megaparsec.newline <|> eof

-------------------------------------------------------------------------------
optional_ :: P a -> P ()
optional_ = option () . void

-------------------------------------------------------------------------------
itemToMetadata :: MetadataItem -> Metadata
itemToMetadata (k, Just v ) = Metadata mempty (Map.singleton k v)
itemToMetadata (k, Nothing) = Metadata (Set.singleton k) mempty

annotateSource :: P (Maybe SourceLocation -> a) -> P a
annotateSource (P (ReaderT r)) = P . ReaderT $ \src -> do
    offset    <- Offset . fromIntegral <$> getOffset
    result    <- r src
    sourceLoc <- maybe (fail sourceLocationError) pure
        $ sourceLocation src offset
    pure . result $ Just sourceLoc

sourceLocationError :: String
sourceLocationError = "UNEXPECTED: Failed to create a source location."
