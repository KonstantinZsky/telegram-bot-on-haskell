module TestInputsTelegram where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

testInputTelegram_empty :: B.ByteString
testInputTelegram_empty = "{\"ok\":true,\"result\":[]}"

testOutputTelegram_empty :: [T.Text]
testOutputTelegram_empty = ["Info: Nothing to output",""]

testInputTelegram_text :: B.ByteString
testInputTelegram_text = "{\"ok\":true,\"result\":[{\"update_id\":389269678,\"message\":{\"message_id\":819,\"from\":{\"id\":447114018,\"is_bot\":false,\"first_name\":\"fn\",\"username\":\"un\",\"language_code\":\"ru\"},\"chat\":{\"id\":447114018,\"first_name\":\"fn\",\"username\":\"un\",\"type\":\"private\"},\"date\":1598048288,\"text\":\"random text\"}}]}"

testOutputTelegram_text :: [T.Text]
testOutputTelegram_text = ["(Key (TelegramSupportData 447114018) FlagText,DataText \"random text\\n\")",""]

testInputTelegram_warning :: B.ByteString
testInputTelegram_warning = "{\"ok\":true,\"result\":[{\"update_id\":389269678,\"message\":{\"message_id\":819,\"from\":{\"id\":447114018,\"is_bot\":false,\"first_name\":\"fn\",\"username\":\"un\",\"language_code\":\"ru\"},\"chat\":{\"id\":447114018,\"first_name\":\"fn\",\"username\":\"un\",\"type\":\"private\"},\"date\":1598048288,\"textXXX\":\"random text\"}}]}"

testOutputTelegram_warning :: [T.Text]
testOutputTelegram_warning = ["Info: Nothing to output","Warning: Unknown format of telegram message, will be ignored: \"{\\\"update_id\\\":389269678,\\\"message\\\":{\\\"textXXX\\\":\\\"random text\\\",\\\"from\\\":{\\\"first_name\\\":\\\"fn\\\",\\\"username\\\":\\\"un\\\",\\\"is_bot\\\":false,\\\"id\\\":447114018,\\\"language_code\\\":\\\"ru\\\"},\\\"chat\\\":{\\\"first_name\\\":\\\"fn\\\",\\\"username\\\":\\\"un\\\",\\\"id\\\":447114018,\\\"type\\\":\\\"private\\\"},\\\"message_id\\\":819,\\\"date\\\":1598048288}}\"",""]

testInputTelegram_help :: B.ByteString
testInputTelegram_help = "{\"ok\":true,\"result\":[{\"update_id\":389269683,\"message\":{\"message_id\":823,\"from\":{\"id\":447114018,\"is_bot\":false,\"first_name\":\"fn\",\"username\":\"un\",\"language_code\":\"ru\"},\"chat\":{\"id\":447114018,\"first_name\":\"fn\",\"username\":\"un\",\"type\":\"private\"},\"date\":1598048675,\"text\":\"/help\",\"entities\":[{\"offset\":0,\"length\":5,\"type\":\"bot_command\"}]}}]}"

testOutputTelegram_help :: [T.Text]
testOutputTelegram_help = ["(Key (TelegramSupportData 447114018) FlagText,DataText \"Help message\\n\")",""]

testInputTelegram_repeat :: B.ByteString
testInputTelegram_repeat = "{\"ok\":true,\"result\":[{\"update_id\":389269684,\"message\":{\"message_id\":824,\"from\":{\"id\":447114018,\"is_bot\":false,\"first_name\":\"fn\",\"username\":\"un\",\"language_code\":\"ru\"},\"chat\":{\"id\":447114018,\"first_name\":\"fn\",\"username\":\"un\",\"type\":\"private\"},\"date\":1598048736,\"text\":\"/repeat\",\"entities\":[{\"offset\":0,\"length\":7,\"type\":\"bot_command\"}]}}]}"

testOutputTelegram_repeat :: [T.Text]
testOutputTelegram_repeat = ["(Key (TelegramSupportData 447114018) FlagButtons,Empty)",""]

testInputTelegram_callback :: B.ByteString
testInputTelegram_callback = "{\"ok\":true,\"result\":[{\"update_id\":389269685,\"callback_query\":{\"id\":\"1920340088788089140\",\"from\":{\"id\":447114018,\"is_bot\":false,\"first_name\":\"fn\",\"username\":\"un\",\"language_code\":\"ru\"},\"message\":{\"message_id\":818,\"from\":{\"id\":1172228691,\"is_bot\":true,\"first_name\":\"Haskell_Test_Bot\",\"username\":\"Haskell_Test_Zsky_Bot\"},\"chat\":{\"id\":447114018,\"first_name\":\"fn\",\"username\":\"un\",\"type\":\"private\"},\"date\":1594303959,\"text\":\"Current repeat count: 4\nChoose the number of bot response duplication.\",\"reply_markup\":{\"inline_keyboard\":[[{\"text\":\"1\",\"callback_data\":\"1\"},{\"text\":\"2\",\"callback_data\":\"2\"},{\"text\":\"3\",\"callback_data\":\"3\"},{\"text\":\"4\",\"callback_data\":\"4\"},{\"text\":\"5\",\"callback_data\":\"5\"}]]}},\"chat_instance\":\"3537552925023664426\",\"data\":\"4\"}}]}"

testOutputTelegram_callback :: [T.Text]
testOutputTelegram_callback = ["Info: Nothing to output",""]

testInputTelegram_all_kinds :: B.ByteString
testInputTelegram_all_kinds = "{\"ok\":true,\"result\":[{\"update_id\":389269678," <>
    "\"message\":{\"message_id\":819,\"from\":{\"id\":447114018,\"is_bot\":false,\"first_name\":\"fn\",\"username\":\"un\",\"language_code\":\"ru\"},\"chat\":{\"id\":447114018,\"first_name\":\"fn\",\"username\":\"un\",\"type\":\"private\"},\"date\":1598048288,\"text\":\"random text\"}},{\"update_id\":389269679," <>
    "\"message\":{\"message_id\":820,\"from\":{\"id\":447114018,\"is_bot\":false,\"first_name\":\"fn\",\"username\":\"un\",\"language_code\":\"ru\"},\"chat\":{\"id\":447114018,\"first_name\":\"fn\",\"username\":\"un\",\"type\":\"private\"},\"date\":1598048428,\"text\":\"/help\",\"entities\":[{\"offset\":0,\"length\":5,\"type\":\"bot_command\"}]}},{\"update_id\":389269680," <>
    "\"message\":{\"message_id\":821,\"from\":{\"id\":447114018,\"is_bot\":false,\"first_name\":\"fn\",\"username\":\"un\",\"language_code\":\"ru\"},\"chat\":{\"id\":447114018,\"first_name\":\"fn\",\"username\":\"un\",\"type\":\"private\"},\"date\":1598048430,\"text\":\"/repeat\",\"entities\":[{\"offset\":0,\"length\":7,\"type\":\"bot_command\"}]}},{\"update_id\":389269681," <>
    "\"callback_query\":{\"id\":\"1920340087974323111\",\"from\":{\"id\":447114018,\"is_bot\":false,\"first_name\":\"fn\",\"username\":\"un\",\"language_code\":\"ru\"},\"message\":{\"message_id\":818,\"from\":{\"id\":1172228691,\"is_bot\":true,\"first_name\":\"Haskell_Test_Bot\",\"username\":\"Haskell_Test_Zsky_Bot\"},\"chat\":{\"id\":447114018,\"first_name\":\"fn\",\"username\":\"un\",\"type\":\"private\"},\"date\":1594303959,\"text\":\"Current repeat count: 4\nChoose the number of bot response duplication.\",\"reply_markup\":{\"inline_keyboard\":[[{\"text\":\"1\",\"callback_data\":\"1\"},{\"text\":\"2\",\"callback_data\":\"2\"},{\"text\":\"3\",\"callback_data\":\"3\"},{\"text\":\"4\",\"callback_data\":\"4\"},{\"text\":\"5\",\"callback_data\":\"5\"}]]}},\"chat_instance\":\"3537552925023664426\",\"data\":\"3\"}},{\"update_id\":389269682," <>
    "\"message\":{\"message_id\":822,\"from\":{\"id\":447114018,\"is_bot\":false,\"first_name\":\"fn\",\"username\":\"un\",\"language_code\":\"ru\"},\"chat\":{\"id\":447114018,\"first_name\":\"fn\",\"username\":\"un\",\"type\":\"private\"},\"date\":1598048506,\"text\":\"another text\"}}]}"

testOutputTelegram_all_kinds :: [T.Text]
testOutputTelegram_all_kinds = ["(Key (TelegramSupportData 447114018) FlagButtons,Empty)","(Key (TelegramSupportData 447114018) FlagText,DataText \"random text\\nHelp message\\nanother text\\nanother text\\nanother text\\n\")",""]

testInputTelegram_error :: B.ByteString
testInputTelegram_error = "testing error responce"

testOutputTelegram_error :: [T.Text]
testOutputTelegram_error = ["Info: Nothing to output","Warning: Unknown format of telegram message, will be ignored: Impossible parsing error while processing bot answer: \"Error in $: string\"",""]