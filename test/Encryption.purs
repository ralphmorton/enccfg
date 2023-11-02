module Test.Encryption (testEncryption) where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..), fromRight, hush, isRight)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Main

testEncryption :: Spec Unit
testEncryption = describe "encryption" do
  it "key generation works" do
    key <- liftEffect (runExceptT genKey)
    isRight key `shouldEqual` true
  it "nonce generation works" do
    nonce <- liftEffect (runExceptT genNonce)
    isRight nonce `shouldEqual` true
  it "values can be encrypted and decrypted" do
    key <- fromRight "" <$> liftEffect (runExceptT genKey)
    let value = "foo"
    ciphertext <- fromRight "" <$> liftEffect (runExceptT $ encrypt { key, value })
    plaintext <- liftEffect (runExceptT $ decrypt { key, value: ciphertext })
    hush plaintext `shouldEqual` (pure value)
