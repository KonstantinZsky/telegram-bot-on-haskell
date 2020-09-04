# Telegram (and VKontakte) bot on haskell

Simple bot made for testing out haskell. No multithreading or multicore. Can work with telegram or vkontakte one at a time.

## Table of сontents
- [Bot capabilities](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#bot-capabilities)
- [Deploy](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#deploy)
  - [Software requirements](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#software-requirements)
  - [Getting bot executable](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#getting-bot-executable)
  - [Running bot executable](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#running-bot-executable)
  - [Client part of the bot](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#client-part-of-the-bot)
    - [Telegram](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#telegram)
    - [VKontakte](https://github.com/KonstantinZsky/telegram-bot-on-haskell/blob/master/README.md#vkontakte)
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

2. Open project folder in the terminal. 
   - Build the project using stack: `stack build`. [How to use stack](https://docs.haskellstack.org/en/stable/GUIDE/)
   - Make executable using stack: `stack install`.

      How it looks in Windows:

<img src="readme%20images/Stack_install.jpg" >

Now you can see where is your executable. For me it is in `C:\Users\Konstantin\AppData\Roaming\local\bin`. Executable is called `telegram-bot-on-haskell-exe.exe`.

### Running bot executable:

Open executable folder in the terminal and launch exe:

<img src="readme%20images/First_launch.jpg" >

Bot couldn't find file config.cfg so it created default config in exe folder. Bot still cant launch because token is not specified. Token is needed to connect to the social network (VK or Telegram). It plays the role of login and password.

In order to get token we must setup the client part of the bot.

### Client part of the bot:

Bot must have representation in the corresponding social network. It needs to have an account with whom user will communicate. Both Telegram and VKonakte have special kind of accounts designed for bots.

#### Telegram

It is very easy to create a bot in Telegram - https://core.telegram.org/bots. You will get token in process. To connect to Telegram you need token.

#### VKontakte

Not so easy for Vkontakte. You must have a user account first and then you need to create a society. After that you need to customize settings of the society and get token and group ID (yes not only token but a group ID too). In order to connect to Vkontakte you need two parametrs - token and group ID.

1. Create user account - https://vk.com

## Config

```
# Mode - social network to connect to: "TG" for telegram, "VK" for vkontakte. Must be in quotes.
cMode = "TG" 

# Verbosity level from wich messagies will show up, can be: "Debug", "Info", "Warning", "Error". Must be in quotes. 
cLogVerbosity = "Debug" 

# Message that will be shown after command /help. Must be in quotes. 
cHelpMessage = "Echo bot, returns messages back to the user." 

# Question that will be shown after command /repeat. Must be in quotes. 
cRepeatQuestion = "Choose the number of bot response duplication." 

# Number of duplication for bot response. 
# Must be greater than 0. 
cRepeatCount = 1 

# Long polling timeout microseconds, 1s = 1000000. For VK must be less than 25s or connection will be lost. 
# Must be greater than 0. 
cPollTimeoutMicroseconds = 5000000 

# Maximum message output per second. For current date (30.06.2020) it is 30 for Telegram and 20 for Vkontakte. 
# Must be greater than 0. 
cMaximumMessageFrequency = 20 

# Bot token for connecting to the API. Must be in quotes. 
cBotToken = "" 

# Vkontakte group ID. Required only for VK mode. 
# Must be greater than 0. 
cGroupID = 1 
```

