module TestInputsVK where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

testInputVK_empty :: B.ByteString
testInputVK_empty = "{\"ts\":\"160\",\"updates\":[]}"

testOutputVK_empty :: [T.Text]
testOutputVK_empty = ["Info: Nothing to output",""]

testInputVK_text :: B.ByteString
testInputVK_text = "{\"ts\":\"161\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"message\":{\"date\":1598057902,\"from_id\":208081,\"id\":272,\"out\":0,\"peer_id\":208081,\"text\":\"test message\",\"conversation_message_id\":272,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"is_hidden\":false},\"client_info\":{\"button_actions\":[\"text\",\"vkpay\",\"open_app\",\"location\",\"open_link\"],\"keyboard\":true,\"inline_keyboard\":true,\"carousel\":false,\"lang_id\":0}},\"group_id\":196994126,\"event_id\":\"fbf7bc54ebf0184a6618fb1f9ea826a89416e442\"}]}"

testOutputVK_text :: [T.Text]
testOutputVK_text = ["(Key (VKSupportData 208081) FlagText,DataText \"test message\\n\")",""]

testInputVK_warning :: B.ByteString
testInputVK_warning = "{\"ts\":\"161\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"message\":{\"date\":1598057902,\"from_id\":208081,\"id\":272,\"out\":0,\"peer_id\":208081,\"textXXX\":\"test message\",\"conversation_message_id\":272,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"is_hidden\":false},\"client_info\":{\"button_actions\":[\"text\",\"vkpay\",\"open_app\",\"location\",\"open_link\"],\"keyboard\":true,\"inline_keyboard\":true,\"carousel\":false,\"lang_id\":0}},\"group_id\":196994126,\"event_id\":\"fbf7bc54ebf0184a6618fb1f9ea826a89416e442\"}]}"

testOutputVK_warning :: [T.Text]
testOutputVK_warning = ["Info: Nothing to output","Warning: Unknown format of vkontakte message, will be ignored: \"{\\\"object\\\":{\\\"client_info\\\":{\\\"lang_id\\\":0,\\\"button_actions\\\":[\\\"text\\\",\\\"vkpay\\\",\\\"open_app\\\",\\\"location\\\",\\\"open_link\\\"],\\\"keyboard\\\":true,\\\"carousel\\\":false,\\\"inline_keyboard\\\":true},\\\"message\\\":{\\\"attachments\\\":[],\\\"textXXX\\\":\\\"test message\\\",\\\"peer_id\\\":208081,\\\"conversation_message_id\\\":272,\\\"random_id\\\":0,\\\"date\\\":1598057902,\\\"from_id\\\":208081,\\\"is_hidden\\\":false,\\\"fwd_messages\\\":[],\\\"id\\\":272,\\\"important\\\":false,\\\"out\\\":0}},\\\"group_id\\\":196994126,\\\"type\\\":\\\"message_new\\\",\\\"event_id\\\":\\\"fbf7bc54ebf0184a6618fb1f9ea826a89416e442\\\"}\"",""]

testInputVK_help :: B.ByteString
testInputVK_help = "{\"ts\":\"162\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"message\":{\"date\":1598058131,\"from_id\":208081,\"id\":273,\"out\":0,\"peer_id\":208081,\"text\":\"\\/help\",\"conversation_message_id\":273,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"is_hidden\":false},\"client_info\":{\"button_actions\":[\"text\",\"vkpay\",\"open_app\",\"location\",\"open_link\"],\"keyboard\":true,\"inline_keyboard\":true,\"carousel\":false,\"lang_id\":0}},\"group_id\":196994126,\"event_id\":\"4e80cc67c716c767e9e48c9e1b84bd96b2c58dc4\"}]}"

testOutputVK_help :: [T.Text]
testOutputVK_help = ["(Key (VKSupportData 208081) FlagText,DataText \"Help message\\n\")",""]

testInputVK_repeat :: B.ByteString
testInputVK_repeat = "{\"ts\":\"163\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"message\":{\"date\":1598058186,\"from_id\":208081,\"id\":274,\"out\":0,\"peer_id\":208081,\"text\":\"\\/repeat\",\"conversation_message_id\":274,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"is_hidden\":false},\"client_info\":{\"button_actions\":[\"text\",\"vkpay\",\"open_app\",\"location\",\"open_link\"],\"keyboard\":true,\"inline_keyboard\":true,\"carousel\":false,\"lang_id\":0}},\"group_id\":196994126,\"event_id\":\"9b9e6f64f3f117d200c04562a817cf7ae3916e39\"}]}"

testOutputVK_repeat :: [T.Text]
testOutputVK_repeat = ["(Key (VKSupportData 208081) FlagButtons,Empty)",""]

testInputVK_callback :: B.ByteString
testInputVK_callback = "{\"ts\":\"168\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"message\":{\"date\":1598058456,\"from_id\":208081,\"id\":281,\"out\":0,\"peer_id\":208081,\"text\":\"3\",\"conversation_message_id\":281,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"payload\":\"{\\\"button\\\":3}\",\"is_hidden\":false},\"client_info\":{\"button_actions\":[\"text\",\"vkpay\",\"open_app\",\"location\",\"open_link\"],\"keyboard\":true,\"inline_keyboard\":true,\"carousel\":false,\"lang_id\":0}},\"group_id\":196994126,\"event_id\":\"bcdf64d1ded01e306c504f5113bb2117db4e476a\"}]}"

testOutputVK_callback :: [T.Text]
testOutputVK_callback = ["Info: Nothing to output",""]

testInputVK_all_kinds :: B.ByteString
testInputVK_all_kinds = "{\"ts\":\"172\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"message\":{\"date\":1598058456,\"from_id\":208081,\"id\":281,\"out\":0,\"peer_id\":208081,\"text\":\"3\",\"conversation_message_id\":281,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"payload\":\"{\\\"button\\\":3}\",\"is_hidden\":false},\"client_info\":{\"button_actions\":[\"text\",\"vkpay\",\"open_app\",\"location\",\"open_link\"],\"keyboard\":true,\"inline_keyboard\":true,\"carousel\":false,\"lang_id\":0}},\"group_id\":196994126,\"event_id\":\"bcdf64d1ded01e306c504f5113bb2117db4e476a\"},{\"type\":\"message_new\",\"object\":{\"message\":{\"date\":1598058582,\"from_id\":208081,\"id\":282,\"out\":0,\"peer_id\":208081,\"text\":\"asd\",\"conversation_message_id\":282,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"is_hidden\":false},\"client_info\":{\"button_actions\":[\"text\",\"vkpay\",\"open_app\",\"location\",\"open_link\"],\"keyboard\":true,\"inline_keyboard\":true,\"carousel\":false,\"lang_id\":0}},\"group_id\":196994126,\"event_id\":\"653052fb2ad467b6d960f9379498a3f017fdc36c\"},{\"type\":\"message_new\",\"object\":{\"message\":{\"date\":1598058584,\"from_id\":208081,\"id\":283,\"out\":0,\"peer_id\":208081,\"text\":\"\\/help\",\"conversation_message_id\":283,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"is_hidden\":false},\"client_info\":{\"button_actions\":[\"text\",\"vkpay\",\"open_app\",\"location\",\"open_link\"],\"keyboard\":true,\"inline_keyboard\":true,\"carousel\":false,\"lang_id\":0}},\"group_id\":196994126,\"event_id\":\"ded3d2d965b7f718137ec8f67f1096d3e15b7eba\"},{\"type\":\"message_new\",\"object\":{\"message\":{\"date\":1598058586,\"from_id\":208081,\"id\":284,\"out\":0,\"peer_id\":208081,\"text\":\"\\/repeat\",\"conversation_message_id\":284,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"is_hidden\":false},\"client_info\":{\"button_actions\":[\"text\",\"vkpay\",\"open_app\",\"location\",\"open_link\"],\"keyboard\":true,\"inline_keyboard\":true,\"carousel\":false,\"lang_id\":0}},\"group_id\":196994126,\"event_id\":\"29eaef90fd39d955bad474da163b495bc4ff4fe5\"},{\"type\":\"message_new\",\"object\":{\"message\":{\"date\":1598058589,\"from_id\":208081,\"id\":285,\"out\":0,\"peer_id\":208081,\"text\":\"bb\",\"conversation_message_id\":285,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"is_hidden\":false},\"client_info\":{\"button_actions\":[\"text\",\"vkpay\",\"open_app\",\"location\",\"open_link\"],\"keyboard\":true,\"inline_keyboard\":true,\"carousel\":false,\"lang_id\":0}},\"group_id\":196994126,\"event_id\":\"8ab70875707b498b299620b41fae8c2a753786fa\"}]}"

testOutputVK_all_kinds :: [T.Text]
testOutputVK_all_kinds = ["(Key (VKSupportData 208081) FlagText,DataText \"asd\\nasd\\nasd\\nHelp message\\nbb\\nbb\\nbb\\n\")","(Key (VKSupportData 208081) FlagButtons,Empty)",""]

testInputVK_error :: B.ByteString
testInputVK_error = "testing error responce"

testOutputVK_error :: [T.Text]
testOutputVK_error = ["Info: Nothing to output","Warning: Unknown format of vkontakte message, will be ignored: Impossible parsing error while processing bot answer: \"Error in $: string\"",""]