{-# LANGUAGE QuasiQuotes #-}

module Web.VK.Templates where

import Data.Aeson
import Data.Aeson.QQ
import Data.Foldable (asum)

buttons_Json :: Value
buttons_Json = [aesonQQ| {one_time: true, buttons: [[
    {action: {type: "text", payload: {button: 1}, label: "1"}, color: "primary"},
    {action: {type: "text", payload: {button: 2}, label: "2"}, color: "primary"},
    {action: {type: "text", payload: {button: 3}, label: "3"}, color: "primary"},
    {action: {type: "text", payload: {button: 4}, label: "4"}, color: "primary"},
    {action: {type: "text", payload: {button: 5}, label: "5"}, color: "primary"}]]}  |] :: Value

buttons_string_raw :: String
buttons_string_raw = "{\"one_time\":true,\"buttons\":[[" <>
    "{\"action\":{\"type\":\"text\",\"payload\":{\"button\":1},\"label\":\"1\"},\"color\":\"primary\"}," <>
    "{\"action\":{\"type\":\"text\",\"payload\":{\"button\":2},\"label\":\"2\"},\"color\":\"secondary\"}," <>
    "{\"action\":{\"type\":\"text\",\"payload\":{\"button\":3},\"label\":\"3\"},\"color\":\"secondary\"}," <>
    "{\"action\":{\"type\":\"text\",\"payload\":{\"button\":4},\"label\":\"4\"},\"color\":\"secondary\"}," <>
    "{\"action\":{\"type\":\"text\",\"payload\":{\"button\":5},\"label\":\"5\"},\"color\":\"secondary\"}]]}"

symbols :: [Char]
symbols = ['!', '\"', '#', '$','%', '&', '\'', '*', '+', ',', ':', ';', '<', '=', '>', '?', '[', ']',
    '^', '`', '{', '|', '}', ' ']

replace :: [String]
replace =["%21", "%22", "%23", "%24", "%25", "%26", "%27", "%2A", "%2B", "%2C", "%3A", "%3B", "%3C", "%3D",
    "%3E", "%3F", "%5B", "%5D", "%5E", "%60", "%7B", "%7C", "%7D", "%20"]

symbols_table :: [(Char,String)]
symbols_table = zip symbols replace

buttons_string_func :: String -> String
buttons_string_func [] = []
buttons_string_func (x:xs) = case asum (map (\(c,str) -> if c == x then Just str else Nothing) symbols_table) of
    Just a -> a <> buttons_string_func xs
    Nothing -> x : buttons_string_func xs

buttons_string :: String
buttons_string = buttons_string_func buttons_string_raw