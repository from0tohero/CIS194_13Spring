{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

let messageType :: String -> MessageType
      messageType = case (take 1 str) of
                      | "I" -> Info
                      | "W" -> Warning
                      | "E" -> Error (take 3 str)
                      | _   -> error "Not recogniced token"
{-}
parseMessage :: String -> LogMessage
parseMessage str = -}