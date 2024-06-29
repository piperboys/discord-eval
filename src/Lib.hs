{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( stateExample
    ) where

import UnliftIO (liftIO)
import Control.Monad (when, void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

getToken :: IO T.Text
getToken = TIO.readFile ".env"

stateExample :: IO ()
stateExample = do
    token <- getToken
    t <- runDiscord $ def {
        discordToken = token
    ,   discordOnEvent = eventHandler
    ,   discordOnStart = liftIO $ putStrLn "Bot starting"
    }

    TIO.putStrLn t

parseMessage :: Message -> (T.Text, [T.Text])
parseMessage m = (T.tail (head content), tail content)
    where content = T.words $ messageContent m

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> when (not (fromBot m) && isCommand m) $ do
        let parsedMessage = parseMessage m
        liftIO $ putStrLn $ "Message received: " <> T.unpack (messageContent m)
        messageHandler m parsedMessage

    GuildCreate g _ -> do
        liftIO $ print $ guildId g

    _ -> return ()

messageHandler :: Message -> (T.Text, [T.Text]) -> DiscordHandler ()
messageHandler m (command, args) = do
        liftIO $ putStrLn $ T.unpack command
        case command of
            "eval" -> do 
                void $ restCall (R.CreateMessage (messageChannelId m) $ T.unwords args )
            _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isCommand :: Message -> Bool
isCommand = ("!" `T.isPrefixOf`) . T.toLower . messageContent
