module Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Foldable (all, elem)
import Data.Newtype (wrap)
import Data.String (joinWith, singleton, split, toCodePointArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (error)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (appendTextFile, exists, readTextFile, truncate, writeTextFile)
import Node.Process (exit')
import Options.Applicative (
  Parser,
  ParserInfo,
  command,
  execParser,
  fullDesc,
  help,
  info,
  long,
  metavar,
  progDesc,
  short,
  strOption,
  subparser
)

--
--
--

data Command
  = KeyGen
  | Encrypt EncryptionOpts
  | Decrypt EncryptionOpts
  | Insert InsertOpts
  | Read ReadOpts

type EncryptionOpts = {
  key :: String,
  value :: String
}

type InsertOpts = {
  key :: String,
  file :: String,
  name :: String,
  value :: String
}

type ReadOpts = {
  key :: String,
  inputFile :: String,
  outputFile :: String
}

--
--
--

main :: Effect Unit
main = do
  cmd <- execParser cmdParser
  run cmd

--
--
--

cmdParser :: ParserInfo Command
cmdParser = info parseCmd $
  fullDesc <>
  progDesc "Encrypted config management"

parseCmd :: Parser Command
parseCmd = subparser $
  command "key-gen" (info (pure KeyGen) (progDesc "Generate an encryption key")) <>
  command "encrypt" (info (Encrypt <$> parseEncryptionOpts) (progDesc "Encrypt a value")) <>
  command "decrypt" (info (Decrypt <$> parseEncryptionOpts) (progDesc "Decrypt a value")) <>
  command "insert" (info (Insert <$> parseInsertOpts) (progDesc "Insert into an encrypted config file")) <>
  command "read" (info (Read <$> parseReadOpts) (progDesc "Read an encrypted config file"))

parseEncryptionOpts :: Parser EncryptionOpts
parseEncryptionOpts = ado
  key <- strOption (long "key" <> short 'k' <> metavar "KEY" <> help "Encryption key")
  value <- strOption (long "value" <> short 'v' <> metavar "VALUE" <> help "Information to encrypt")
  in { key, value }

parseInsertOpts :: Parser InsertOpts
parseInsertOpts = ado
  key <- strOption (long "key" <> short 'k' <> metavar "KEY" <> help "Encryption key")
  file <- strOption (long "file" <> short 'f' <> metavar "FILE" <> help "Config file")
  name <- strOption (long "name" <> short 'n' <> metavar "NAME" <> help "Name")
  value <- strOption (long "value" <> short 'v' <> metavar "VALUE" <> help "Information to encrypt")
  in { key, file, name, value }

parseReadOpts :: Parser ReadOpts
parseReadOpts = ado
  key <- strOption (long "key" <> short 'k' <> metavar "KEY" <> help "Encryption key")
  inputFile <- strOption (long "in" <> short 'i' <> metavar "INPUT_FILE" <> help "Input (encrypted) config file")
  outputFile <- strOption (long "out" <> short 'o' <> metavar "OUTPUT_FILE" <> help "Output config file")
  in { key, inputFile, outputFile }

--
--
--

run :: Command -> Effect Unit
run = case _ of
  KeyGen ->
    log =<< genRand_ 32
  Encrypt opts ->
    log =<< encrypt opts
  Decrypt opts -> do
    res <- decrypt opts
    case res of
      Decrypted plaintext ->
        log plaintext
      InvalidValue ->
        invalidValue
  Insert opts ->
    insert opts
  Read opts ->
    read opts

invalidValue :: Effect Unit
invalidValue = do
  log "Invalid value"
  exit' 1

--

encrypt :: EncryptionOpts -> Effect String
encrypt { key, value } = do
  nonce <- genRand_ 16
  ciphertext <- encrypt_ nonce key value
  pure (nonce <> ":" <> ciphertext)

foreign import encrypt_ :: String -> String -> String -> Effect String

--

data DecryptResult
  = Decrypted String
  | InvalidValue

decrypt :: EncryptionOpts -> Effect DecryptResult
decrypt { key, value: salted } = do
  case split (wrap ":") salted of
    [nonce, value] ->
      Decrypted <$> decrypt_ nonce key value
    _ -> do
      pure InvalidValue

foreign import decrypt_ :: String -> String -> String -> Effect String

--

insert :: InsertOpts -> Effect Unit
insert { key, file, name, value } = case isValidName name of
  false -> do
    log "Invalid name (only uppercase characters and _ are allowed)"
    exit' 1
  true -> do
    ensureFile file
    ciphertext <- encrypt { key, value }
    appendTextFile UTF8 file ("\n" <> name <> " " <> ciphertext)

isValidName :: String -> Boolean
isValidName = all (flip elem allowed <<< singleton) <<< toCodePointArray
  where
  allowed = [
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
    "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T",
    "U", "V", "W", "X", "Y", "Z",
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
    "_"
  ]

--

read :: ReadOpts -> Effect Unit
read { key, inputFile, outputFile } = do
  inputLines <- split (wrap "\n") <$> readTextFile UTF8 inputFile
  outputLines <- traverse (map toReadOutputLine <<< decryptLine key) inputLines
  let output = joinWith "\n" outputLines
  ensureFile outputFile
  truncate outputFile 0
  writeTextFile UTF8 outputFile output

toReadOutputLine :: Either String (Tuple String String) -> String
toReadOutputLine = case _ of
  Left line ->
    line
  Right (Tuple k v) ->
    k <> "=\"" <> v <> "\""

--

ensureFile :: String -> Effect Unit
ensureFile path = unlessM (exists path) $
  writeTextFile UTF8 path ""

decryptLine :: String -> String -> Effect (Either String (Tuple String String))
decryptLine key line = case split (wrap " ") line of
  [name, value] -> do
    res <- decrypt { key, value }
    case res of
      Decrypted plaintext ->
        pure $ pure (Tuple name plaintext)
      InvalidValue -> do
        invalidValue
        throwError (error "Invalid value in file")
  _ ->
    pure (Left line)

foreign import genRand_ :: Int -> Effect String
