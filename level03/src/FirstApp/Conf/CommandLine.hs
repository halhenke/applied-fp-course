module FirstApp.Conf.CommandLine
  ( commandLineParser
  ) where

-- | Command Line Parsing

-- We will use the ``optparse-applicative`` package to build our command line
-- parser. As this particular problem is fraught with silly dangers and we
-- appreciate someone else having eaten this gremlin on our behalf.

-- You'll need to use the documentation for ``optparse-applicative`` to help you
-- write these functions as we're relying on their API to produce the types we
-- need. We've provided some of the less interesting boilerplate for you.
commandLineParser
  :: ParserInfo PartialConf
commandLineParser =
  let mods = fullDesc
        <> progDesc "Manage comments for something"
        <> header "Your first Haskell app!"
  in
    info (helper <*> partialConfParser) mods

-- Combine the smaller parsers into our larger ``PartialConf`` type.
partialConfParser
  :: Parser PartialConf
partialConfParser = PartialConf
  <$> portParser
  <*> helloMsgParser

-- Parse the Port value off the command line args and into a Last wrapper.
portParser
  :: Parser (Last Port)
portParser =
  let
    mods = long "port"
           <> short 'p'
           <> metavar "PORT"
           <> help "TCP Port to accept requests on"
    portReader =
      eitherReader (fmap Port . readEither)
  in
    Last <$> optional (option portReader mods)

-- Parse the HelloMsg from the input string into our type and into a Last wrapper.
helloMsgParser
  :: Parser (Last HelloMsg)
helloMsgParser =
  let
    mods = long "hello-msg"
           <> short 'm'
           <> metavar "HELLOMSG"
           <> help "Message to respond to requests with."
  in
    Last <$> optional (HelloMsg . fromString <$> strOption mods)
