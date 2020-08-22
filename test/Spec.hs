module Main where

import qualified Data.Text.IO   as T
import qualified Data.Text      as T 
import qualified Data.ByteString.Lazy as B

import TestInstances.TestRun
import TestInputsTelegram
import TestInputsVK

testToPastTelegram :: B.ByteString -> [T.Text] -> String -> IO ()
testToPastTelegram input output name = if (testRunTelegram [input]) == output then putStrLn $ (addSpaces name) <> 
    ": passed" else putStrLn $ (addSpaces name) <> ": failed" where
        addSpaces str = if str_length < 15 then str <> (replicate (15 - str_length) ' ') else str where
            str_length = length str 

testToPastVK :: B.ByteString -> [T.Text] -> [Char] -> IO ()
testToPastVK input output name = if (testRunVK [input]) == output then putStrLn $ (addSpaces name) <> 
    ": passed" else putStrLn $ (addSpaces name) <> ": failed" where
        addSpaces str = if str_length < 15 then str <> (replicate (15 - str_length) ' ') else str where
            str_length = length str 

main :: IO ()
main = do
    putStrLn "Telegram testing start"
    testToPastTelegram testInputTelegram_error      testOutputTelegram_error        "Wrong format"
    testToPastTelegram testInputTelegram_warning    testOutputTelegram_warning      "Unknown"    
    testToPastTelegram testInputTelegram_empty      testOutputTelegram_empty        "Empty"
    testToPastTelegram testInputTelegram_text       testOutputTelegram_text         "Text"
    testToPastTelegram testInputTelegram_help       testOutputTelegram_help         "Help"
    testToPastTelegram testInputTelegram_repeat     testOutputTelegram_repeat       "Repeat"
    testToPastTelegram testInputTelegram_callback   testOutputTelegram_callback     "Callback"
    testToPastTelegram testInputTelegram_all_kinds  testOutputTelegram_all_kinds    "All kinds"
    putStrLn ""
    putStrLn "VK testing start"
    testToPastVK testInputVK_error      testOutputVK_error        "Wrong format"
    testToPastVK testInputVK_warning    testOutputVK_warning      "Unknown"    
    testToPastVK testInputVK_empty      testOutputVK_empty        "Empty"
    testToPastVK testInputVK_text       testOutputVK_text         "Text"
    testToPastVK testInputVK_help       testOutputVK_help         "Help"
    testToPastVK testInputVK_repeat     testOutputVK_repeat       "Repeat"
    testToPastVK testInputVK_callback   testOutputVK_callback     "Callback"
    testToPastVK testInputVK_all_kinds  testOutputVK_all_kinds    "All kinds"



