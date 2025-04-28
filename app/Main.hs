{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Main where

import           Control.Monad (when, void)
import           UnliftIO.Concurrent
import           Data.Text
import qualified Data.Text.IO as TIO

import           Discord
import           Discord.Types
import qualified Discord.Requests as R

import           Crypto.Cipher.AES (AES128)
import           Crypto.Cipher.Types
import           Crypto.Error
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base64 as B64
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as B64

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

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
    key <- BS.readFile "key"
    encryptedTokenBase64 <- BS.readFile "discord_token" -- encrypted in base64

    putStrLn $ "Clé: " ++ C8.unpack key
    putStrLn $ "Token chiffré (base64): " ++ C8.unpack encryptedTokenBase64

    -- Décodage du token base64 -> binaire
    encryptedToken <- decodeToken $ B64.decode encryptedTokenBase64
    let tokenBS = decryptAES key encryptedToken  -- ByteString
    let tokenText = decodeUtf8 tokenBS  -- ByteString -> Text (utf8)

    putStrLn $ "Token déchiffré: " ++ show tokenText

    userFacingError <- runDiscord $ def
            { discordToken = tokenText
            , discordOnEvent = eventHandler
            , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
            }

    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)


decodeToken :: Either String B64.ByteString -> IO B64.ByteString
decodeToken result = case result of
    Left err   -> error ("Erreur de décodage base64: " ++ err)
    Right tok -> return tok


eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> when (isPing m && not (fromBot m)) $ do
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
        threadDelay (2 * 10 ^ (6 :: Int))
        void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent

main ::  IO ()
main = pingpongExample