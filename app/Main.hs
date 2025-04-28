{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Main where

import           Control.Monad (when, void)
import           UnliftIO.Concurrent ( threadDelay )
import           Data.Text ( isPrefixOf, toLower )
import qualified Data.Text.IO as TIO

import qualified Discord as D
import qualified Discord.Types as DTs
import qualified Discord.Requests as R

import           Crypto.Cipher.AES (AES128)
import           Crypto.Cipher.Types ( Cipher(cipherInit), BlockCipher(ctrCombine), nullIV )
import           Crypto.Error ( CryptoFailable(CryptoFailed, CryptoPassed) )

import           Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B64

-- consts
encryptedTokenFilePath :: FilePath
encryptedTokenFilePath = "discord_token"
keyFilePath :: FilePath
keyFilePath = "key"

-- - AES encryption setup
-- Fonction pour initialiser le cipher avec une clé
initAES :: BS.ByteString -> AES128
initAES key =
  case cipherInit key of
    CryptoPassed cipher -> cipher
    CryptoFailed err    -> error (show err)
-- Chiffrement
encryptAES :: BS.ByteString -> BS.ByteString -> BS.ByteString
encryptAES key plaintext =
  let cipher = initAES key
      iv = nullIV  -- IV nul juste pour l'exemple
  in ctrCombine cipher iv plaintext
-- Déchiffrement (symétrique avec CTR mode)¨
decryptAES :: BS.ByteString -> BS.ByteString -> BS.ByteString
decryptAES = encryptAES  -- même fonction en CTR mode
-- Either -> plain ByteString
decodeToken :: Either String B64.ByteString -> B64.ByteString
decodeToken result = case result of
    Left err   -> error ("Erreur de décodage base64: " ++ err)
    Right tok -> tok

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
    key <- BS.readFile keyFilePath

    -- Décodage du token base64
    encryptedToken <- decodeToken . B64.decode <$> BS.readFile encryptedTokenFilePath
    let tokenBS = decryptAES key encryptedToken  -- ByteString
    let tokenText = decodeUtf8 tokenBS  -- ByteString -> Text (utf8)

    putStrLn $ "Discord Token succesfully decrypted: " ++ show tokenText

    userFacingError <- D.runDiscord $ D.def
            { D.discordToken = tokenText
            , D.discordOnEvent = eventHandler
            , D.discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
            }

    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)


eventHandler :: DTs.Event -> D.DiscordHandler ()
eventHandler event = case event of
    DTs.MessageCreate m -> when (isPing m && not (fromBot m)) $ do
        void $ D.restCall (R.CreateReaction (DTs.messageChannelId m, DTs.messageId m) "eyes")
        threadDelay (10 ^ (5 :: Int))
        void $ D.restCall (R.CreateMessage (DTs.messageChannelId m) "Pong!")
    _ -> return ()

fromBot :: DTs.Message -> Bool
fromBot = DTs.userIsBot . DTs.messageAuthor

isPing :: DTs.Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . DTs.messageContent

main ::  IO ()
main = pingpongExample