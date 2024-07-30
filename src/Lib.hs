{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( stateExample
  ) where

import Control.Monad (void, when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readCreateProcess, shell)
import UnliftIO (liftIO)

import Discord
import qualified Discord.Requests as R
import Discord.Types

getToken :: IO T.Text
getToken = TIO.readFile ".env"

stateExample :: IO ()
stateExample = do
  token <- getToken

  t <-
    runDiscord $
      def
        { discordToken = token
        , discordOnEvent = eventHandler
        , discordOnStart = liftIO $ putStrLn "Bot starting"
        }

  TIO.putStrLn t

sanitize :: String -> String
sanitize = tail . init

format :: String -> String
format s = "```python\n" ++ unlines (drop 3 $ lines s) ++ "\n```"

parseMessage :: Message -> (T.Text, [T.Text])
parseMessage m = (T.tail (head content), tail content)
  where
    content = T.words $ T.replace "`" "" (messageContent m)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (not (fromBot m) && isCommand m) $ do
    let parsedMessage = parseMessage m
    liftIO $ putStrLn $ "Command received: " <> T.unpack (messageContent m)
    messageHandler m parsedMessage
  GuildCreate g _ -> do
    liftIO $ print $ guildId g
  _ -> return ()

messageHandler :: Message -> (T.Text, [T.Text]) -> DiscordHandler ()
messageHandler m (command, args) = do
  case command of
    "eval" -> do
      let input = sanitize (show (T.unpack (T.replace ";" "\n" (T.unwords args))))
      output <- liftIO $ readCreateProcess (shell $ "echo '" ++ input ++ "' | piper -repl") ""
      void $ restCall (R.CreateMessage (messageChannelId m) $ T.pack (format output))
    "args" -> do
      let input = sanitize (show (T.unpack (T.replace ";" "\n" (T.unwords args))))
      liftIO $ print args
      liftIO $ putStrLn input

      void $ restCall (R.CreateMessage (messageChannelId m) $ T.pack input)
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isCommand :: Message -> Bool
isCommand = ("!" `T.isPrefixOf`) . T.toLower . messageContent
