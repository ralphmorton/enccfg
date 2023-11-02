module Main where

import Prelude

import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Foldable (all, elem)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (joinWith, singleton, split, toCodePointArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error)
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
import Options.Applicative.Types (optional)

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
  outputFile :: Maybe String
}

data Err
  = KeyGenError Error
  | NonceGenError Error
  | InvalidValue String
  | EncryptionError Error
  | DecryptionError Error
  | InvalidVariableName String

instance Show Err where
  show = case _ of
    KeyGenError e ->
      "KeyGenError " <> show e
    NonceGenError e ->
      "NonceGenError " <> show e
    InvalidValue name ->
      "InvalidValue " <> show name
    EncryptionError e ->
      "EncryptionError " <> show e
    DecryptionError e ->
      "DecryptionError " <> show e
    InvalidVariableName name ->
      "InvalidVariableName " <> show name

type App = ExceptT Err Effect

--
--
--

main :: Effect Unit
main = do
  cmd <- execParser cmdParser
  res <- runExceptT (run cmd)
  case res of
    Right output -> do
      log output
      exit' 0
    Left e -> do
      log (errorOutput e)
      exit' 1

errorOutput :: Err -> String
errorOutput = case _ of
  KeyGenError e ->
    "Key generation failed: " <> show e
  NonceGenError e ->
    "Nonce generation failed: " <> show e
  InvalidValue name ->
    "Invalid encryped value " <> show name
  EncryptionError e ->
    "Encryption failed: " <> show e
  DecryptionError e ->
    "Decryption failed: " <> show e
  InvalidVariableName name ->
    "Invalid name " <> show name <> " (only uppercase characters and _ are allowed)"

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
  outputFile <- optional $ strOption (long "out" <> short 'o' <> metavar "OUTPUT_FILE" <> help "Output config file")
  in { key, inputFile, outputFile }

--
--
--

run :: Command -> ExceptT Err Effect String
run = case _ of
  KeyGen ->
    genKey
  Encrypt opts ->
    encrypt opts
  Decrypt opts ->
    decrypt opts
  Insert opts ->
    insert opts
  Read opts ->
    read opts

invalidValue :: Effect Unit
invalidValue = do
  log "Invalid value"
  exit' 1

--

genKey :: App String
genKey = failWith KeyGenError (genRand_ 32)

--

genNonce :: App String
genNonce = failWith NonceGenError (genRand_ 16)

--

encrypt :: EncryptionOpts -> App String
encrypt { key, value } = do
  nonce <- genNonce
  ciphertext <- failWith EncryptionError (encrypt_ nonce key value)
  pure (nonce <> ":" <> ciphertext)

foreign import encrypt_ :: String -> String -> String -> Effect String

--

decrypt :: EncryptionOpts -> App String
decrypt { key, value: salted } = do
  case split (wrap ":") salted of
    [nonce, value] ->
      failWith DecryptionError (decrypt_ nonce key value)
    _ ->
      throwError (InvalidValue salted)

foreign import decrypt_ :: String -> String -> String -> Effect String

--

insert :: InsertOpts -> App String
insert { key, file, name, value } = case isValidName name of
  false ->
    throwError (InvalidVariableName name)
  true -> do
    lift (ensureFile file)
    ciphertext <- encrypt { key, value }
    lift $ appendTextFile UTF8 file ("\n" <> name <> " " <> ciphertext)
    pure ""

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

read :: ReadOpts -> App String
read { key, inputFile, outputFile } = do
  inputLines <- split (wrap "\n") <$> lift (readTextFile UTF8 inputFile)
  outputLines <- traverse (map toReadOutputLine <<< decryptLine key) inputLines
  let output = joinWith "\n" outputLines
  case outputFile of
    Nothing ->
      pure output
    Just outputFile' -> do
      lift do
        ensureFile outputFile'
        truncate outputFile' 0
        writeTextFile UTF8 outputFile' output
      pure ""

toReadOutputLine :: Either String (Tuple String String) -> String
toReadOutputLine = case _ of
  Left line ->
    line
  Right (Tuple k v) ->
    k <> "=\"" <> v <> "\""

--

failWith :: forall a. (Error -> Err) -> Effect a -> App a
failWith e f = do
  res <- lift (try f)
  case res of
    Left err ->
      throwError (e err)
    Right a ->
      pure a

ensureFile :: String -> Effect Unit
ensureFile path = unlessM (exists path) $
  writeTextFile UTF8 path ""

decryptLine :: String -> String -> App (Either String (Tuple String String))
decryptLine key line = case split (wrap " ") line of
  [name, value] -> do
    plaintext <- decrypt { key, value }
    pure $ pure (Tuple name plaintext)
  _ ->
    pure (Left line)

foreign import genRand_ :: Int -> Effect String
