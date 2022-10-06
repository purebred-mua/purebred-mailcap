{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Mailcap.Command
  ( prepCommand
  , ShellCommand
  , ShellCommandStdin(..)
  , ShellCommandReplacementActions(..)
  ) where

import Control.Applicative ((<|>), many, optional)
import Control.Monad.State

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import Data.RFC1524.Internal (ContentType)

data ShellCommandStdin = BodyOnStdin | NoStdin
  deriving (Eq, Show)

-- | The body part will be passed to the command as standard input
-- unless one or more instances of @%s@ appear in the command.
--
instance Semigroup ShellCommandStdin where
  BodyOnStdin <> r = r
  NoStdin     <> _ = NoStdin

-- | @mempty = 'BodyOnStdin'@
instance Monoid ShellCommandStdin where
  mempty = BodyOnStdin

data ShellCommand = ShellCommand
  ShellCommandStdin   -- ^ what to send on stdin
  [FilePath]          -- ^ list of file names in @%@ substitutions
  String              -- ^ command string
  deriving (Show)
  -- TODO should we use (Set FilePath) instead?

instance Semigroup ShellCommand where
  ShellCommand a b c <> ShellCommand x y z =
    ShellCommand (a <> x) (b <> y) (c <> z)

instance Monoid ShellCommand where
  mempty = ShellCommand mempty mempty mempty

-- | Compute the final shell command, performing @%@ replacements
-- as needed.  Replacement actions are executed at most once,
-- except named parameters which are executed on every occurrence.
--
-- The @[FilePath]@ in the result may contain duplicates.
--
prepCommand
  :: (Monad m)
  => B.ByteString
  -> ShellCommandReplacementActions m
  -> m (Either String ShellCommand)
prepCommand pat dict =
  evalStateT
    ( sequenceA . fmap ($ caching dict)
      $ parseOnly (parseCommand <* endOfInput) pat )
    ( ShellCommandReplacementActions
        Nothing Nothing Nothing Nothing (const Nothing) )

-- | Actions to get values for @%@ replacements in the shell
-- command.  When used with 'prepCommand', each action will be
-- executed at most once, except 'getNamedParameters' which is
-- executed at every occurence of a @%{<param-name>}@.
--
data ShellCommandReplacementActions m = ShellCommandReplacementActions
  { getBodyFile :: m FilePath
  , getSubpartFiles :: m [(ContentType, FilePath)]
  , getSubpartCount :: m Int
  , getContentType :: m ContentType
  , getNamedParameter :: String -> m (Maybe String)  {- text? -}
  }

caching
  :: forall m. (Monad m)
  => ShellCommandReplacementActions m
  -> ShellCommandReplacementActions (StateT (ShellCommandReplacementActions Maybe) m)
caching dict =
  ShellCommandReplacementActions
    ( go getBodyFile     (\a s -> s { getBodyFile = a }) )
    ( go getSubpartFiles (\a s -> s { getSubpartFiles = a }) )
    ( go getSubpartCount (\a s -> s { getSubpartCount = a }) )
    ( go getContentType  (\a s -> s { getContentType = a }) )
    ( \k -> lift (getNamedParameter dict k) )  -- TODO cache?
  where
    go
      :: (forall m1. ShellCommandReplacementActions m1 -> m1 a)
      -> (Maybe a -> ShellCommandReplacementActions Maybe -> ShellCommandReplacementActions Maybe)
      -> StateT (ShellCommandReplacementActions Maybe) m a
    go r w = do
      s <- get
      maybe (lift (r dict) >>= \a -> a <$ put (w (Just a) s)) pure (r s)

parseCommand
  :: (Applicative m)
  => Parser (ShellCommandReplacementActions m -> m ShellCommand)
parseCommand =
  fmap (fmap mconcat . sequenceA) . sequenceA
  <$> many (parseEscape <|> parseReplacement <|> parsePlain)

parsePlain
  :: (Applicative m)
  => Parser (ShellCommandReplacementActions m -> m ShellCommand)
parsePlain =
  (pure . pure . (ShellCommand BodyOnStdin []) . B8.unpack)
  <$> takeWhile1 (\c -> c /= '%' && c /= '\\')

parseEscape
  :: (Applicative m)
  => Parser (ShellCommandReplacementActions m -> m ShellCommand)
parseEscape = do
  _ <- char '\\'
  c <- anyChar
  (pure . pure . pure) (ShellCommand BodyOnStdin [] [c])

parseReplacement
  :: (Applicative m)
  => Parser (ShellCommandReplacementActions m -> m ShellCommand)
parseReplacement = do
  _ <- char '%'
  c <- optional anyChar
  case c of
    Nothing ->  -- '%' at end of input
      (pure . pure . pure) (ShellCommand BodyOnStdin [] "%")
    Just 's' ->
      pure (fmap (\path -> ShellCommand NoStdin [path] path) . getBodyFile)
    Just 't' ->
      pure (fmap (undefined {- TODO -}) . getContentType)
    Just 'F' ->
      pure (fmap (undefined {- TODO -}) . getSubpartFiles)
    Just 'n' ->
      pure (fmap (ShellCommand BodyOnStdin [] . show) . getSubpartCount)
    Just '{' -> do
      paramName <- B8.unpack <$> takeTill (== '}')
      _ <- char '}'
      pure $ \dict ->
        ShellCommand BodyOnStdin [] . maybe "" id
        <$> getNamedParameter dict paramName
    Just c' ->
      (pure . pure . pure) (ShellCommand BodyOnStdin [] ['%',c'])
