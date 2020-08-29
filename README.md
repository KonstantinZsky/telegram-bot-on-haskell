# Telegram (and VKontakte) bot on haskell

Simple bot made for testing out haskell. No multithreading or multicore. Can work with telegram or vkontakte one at a time.

## Table of сontents
- [Bot capabilities](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#bot-capabilities)
- [Deploy](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#deploy)
- [Config](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#config)

## Bot capabilities

1. Message echo
   - Bot will return the user his message. You write a fraze to bot and he responds with the same fraze.
   - Bot can repeat message multiple times when answering. This is configured with the commands.
2. Commands
   - `/help` - shows information about the bot. Message is set in config.
   - `/repeat` - calls keyboard to select the number of repeats in the response. It can be numbers from 1 to 5.
     - VK:
     
         <img src="readme%20images/Repeat_Telegram.jpg" width="500" >
     
     - Telegram:
     
         <img src="readme%20images/Repeat_VK.jpg" width="500" >

## Deploy

1. Copy this project using git: `$ git clone git@github.com:KonstantinZsky/telegram-bot-on-haskell.git`.  [How to use git](https://git-scm.com/book/en/v2) 


## Config
