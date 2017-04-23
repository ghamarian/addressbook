module Main where

import Options.Applicative
import Data.Monoid


data Command = Add String String |
               Phone String String |
               Show String |
               Email String String
               deriving (Eq, Show)

parserAdd :: Parser Command
parserAdd = Add <$> strArgument (metavar "FILENAME")
                <*> strArgument (metavar "PERSON_NAME")

parserEmail ::Parser Command
parserEmail = Email <$> strArgument (metavar "FILENAME")
                    <*> strArgument (metavar "EMAIL_ADDRESS")

parserShow :: Parser Command
parserShow = Show <$> strArgument (metavar "FILENAME")

parserPhone :: Parser Command
parserPhone = Phone <$> strArgument (metavar "FILENAME")
                    <*> strArgument (metavar "PHONE")

parserCommand :: Parser Command
parserCommand = subparser $
          command "add" (parserAdd `withInfo` "Add an entery.") <>
          command "email" (parserEmail `withInfo` "Add email addresss.") <>
          command "phone" (parserPhone `withInfo` "Add phone number.") <>
          command "show" (parserShow `withInfo` "Show record.")

parserInfoCommand :: ParserInfo Command
parserInfoCommand = info parserCommand (progDesc "Manage address book.")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

main  :: IO ()
main = do
       command <- execParser parserInfoCommand
       print command
