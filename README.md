# Telegram (and VKontakte) bot on haskell

Simple bot made for testing out haskell. No multithreading or multicore. Can work with telegram or vkontakte one at a time.

## Table of сontents
- [Bot capabilities](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#bot-capabilities)
- [Deploy](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#deploy)
  - [Software requirements](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#software-requirements)
  - [Getting bot executable](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#getting-bot-executable)
  - [Running bot executable](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#running-bot-executable)
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

### Software requirements:
    
- git - [how to use git](https://git-scm.com/book/en/v2) 
- haskell platform - https://www.haskell.org/platform/ which includes:
  - stack
  - cabal
  - ghc

### Getting bot executable:

1. Copy this project to your computer using git: `$ git clone git@github.com:KonstantinZsky/telegram-bot-on-haskell.git`.  

2. Open project folder in your terminal. 
   - Build the project using stack: `stack build`. [How to use stack](https://docs.haskellstack.org/en/stable/GUIDE/)
   - Make executable using stack: `stack install`.

      How it looks in Windows:

      <img src="readme%20images/Stack_install.jpg" >

      Now you can see where is your executable. For me it is in `C:\Users\Konstantin\AppData\Roaming\local\bin`. Executable called `telegram-bot-on-haskell-exe.exe`.

### Running bot executable:

## Config
