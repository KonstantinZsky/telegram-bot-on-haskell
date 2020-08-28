# Telegram (and VKontakte) bot on haskell

Simple bot made for testing haskell capabilities. No multithreading or multicore. Can work with telegram or vkontakte one at a time.

## Bot capabilities

1. Message echo
   - Bot will return the user his message. You write a fraze to bot and he responds with the same fraze.
   - Bot can repeat message multiple times when answering. This is configured with the commands.
2. Commands
   - `/help` - shows information about the bot. Message is set in config.
   - `/repeat` - calls keyboard to select the number of repeats in the response.
     - VK : 
