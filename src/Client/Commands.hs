{-# LANGUAGE BangPatterns, OverloadedStrings, ExistentialQuantification #-}

{-|
Module      : Client.Commands
Description : Implementation of slash commands
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module renders the lines used in the channel mask list. A mask list
can show channel bans, quiets, invites, and exceptions.
-}

module Client.Commands
  ( CommandResult(..)
  , execute
  , executeUserCommand
  , commandExpansion
  , tabCompletion
  -- * Commands
  , CommandSection(..)
  , Command(..)
  , CommandImpl(..)
  , commands
  , commandsList
  ) where

import           Client.CApi
import           Client.Commands.Arguments
import           Client.Commands.Exec
import           Client.Commands.Interpolation
import           Client.Commands.Recognizer
import           Client.Commands.WordCompletion
import           Client.Configuration
import           Client.Message
import           Client.State
import           Client.State.Channel
import qualified Client.State.EditBox as Edit
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window
import           Control.Applicative
import           Control.Exception (displayException, try)
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.HashSet (HashSet)
import           Data.List (nub, (\\))
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.List.Split
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid ((<>))
import           Data.Time
import           Irc.Commands
import           Irc.Identifier
import           Irc.RawIrcMsg
import           Irc.Message
import           Irc.UserInfo
import           Irc.Modes
import           LensUtils
import           RtsStats (getStats)
import           System.Process
import           Text.Read
import           Text.Regex.TDFA
import           Text.Regex.TDFA.String (compile)

-- | Possible results of running a command
data CommandResult
  -- | Continue running the client, consume input if command was from input
  = CommandSuccess ClientState
  -- | Continue running the client, report an error
  | CommandFailure ClientState
  -- | Client should close
  | CommandQuit ClientState


-- | Type of commands that always work
type ClientCommand a = ClientState -> a {- ^ arguments -} -> IO CommandResult

-- | Type of commands that require an active network to be focused
type NetworkCommand a = NetworkState {- ^ current network -} -> ClientCommand a

-- | Type of commands that require an active channel to be focused
type ChannelCommand a = Identifier {- ^ focused channel -} -> NetworkCommand a


-- | Pair of implementations for executing a command and tab completing one.
-- The tab-completion logic is extended with a bool
-- indicating that tab completion should be reversed
data CommandImpl a
  -- | no requirements
  = ClientCommand  (ClientCommand  a) (Bool -> ClientCommand  String)
  -- | requires an active network
  | NetworkCommand (NetworkCommand a) (Bool -> NetworkCommand String)
  -- | requires an active chat window
  | ChatCommand    (ChannelCommand a) (Bool -> ChannelCommand String)
  -- | requires an active channel window
  | ChannelCommand (ChannelCommand a) (Bool -> ChannelCommand String)


-- | A command is a list of aliases, an argument specification, implementation,
-- and documentation. The arguments and implementation must match so that
-- the parsed arguments will match what the implementation expects.
data Command = forall a. Command
  { -- | Names of this command, first in the list is the "primary" name
    cmdNames          :: NonEmpty Text
  -- | Specification of the arguments of the command
  , cmdArgumentSpec   :: ArgumentSpec a
  -- | Multi-line IRC-formatted documentation text used for @/help@
  , cmdDocumentation  :: Text
  -- | Implementation of the command for both execution and tab completion
  , cmdImplementation :: CommandImpl a
  }

-- | A command section is a logical grouping of commands. This allows for
-- showing more structure in the help menu system.
data CommandSection = CommandSection
  { cmdSectionName :: Text
  , cmdSectionCmds :: [Command]
  }

-- | Consider the text entry successful and resume the client
commandSuccess :: Monad m => ClientState -> m CommandResult
commandSuccess = return . CommandSuccess

-- | Consider the text entry a failure and resume the client
commandFailure :: Monad m => ClientState -> m CommandResult
commandFailure = return . CommandFailure

-- | Command failure with an error message printed to client window
commandFailureMsg :: Text -> ClientState -> IO CommandResult
commandFailureMsg e st =
  return $! CommandFailure $! set clientErrorMsg (Just e) st

-- | Interpret the given chat message or command. Leading @/@ indicates a
-- command. Otherwise if a channel or user query is focused a chat message
-- will be sent.
execute ::
  String {- ^ chat or command -} ->
  ClientState -> IO CommandResult
execute str st =
  case str of
    []          -> commandFailure st
    '/':command -> executeUserCommand Nothing command st
    msg         -> executeChat msg st

-- | Execute command provided by user, resolve aliases if necessary.
executeUserCommand :: Maybe Text -> String -> ClientState -> IO CommandResult
executeUserCommand discoTime command st = do
  let key = Text.takeWhile (/=' ') tcmd
      rest = dropWhile (==' ') . dropWhile (/=' ') $ command

  case views (clientConfig . configMacros) (recognize key) st of
    Exact (Macro (MacroSpec spec) cmdExs) ->
      case parseArguments spec rest *> traverse resolveMacro cmdExs of
        Nothing   -> commandFailureMsg "macro expansions failed" st
        Just cmds -> process cmds st
    _ -> executeCommand Nothing command st
  where
    resolveMacro = resolveMacroExpansions (commandExpansion discoTime st) expandInt

    tcmd = Text.pack command
    args = Text.words tcmd

    expandInt i = preview (ix (fromInteger i)) args

    process [] st0 = commandSuccess st0
    process (c:cs) st0 =
      do res <- executeCommand Nothing (Text.unpack c) st0
         case res of
           CommandSuccess st1 -> process cs st1
           CommandFailure st1 -> process cs st1 -- ?
           CommandQuit st1    -> return (CommandQuit st1)

-- | Compute the replacement value for the given expansion variable.
commandExpansion ::
  Maybe Text  {- ^ disconnect time    -} ->
  ClientState {- ^ client state       -} ->
  Text        {- ^ expansion variable -} ->
  Maybe Text  {- ^ expansion value    -}
commandExpansion discoTime st v =
  case v of
    "network" -> views clientFocus focusNetwork st
    "channel" -> previews (clientFocus . _ChannelFocus . _2) idText st
    "nick"    -> do net <- views clientFocus focusNetwork st
                    cs  <- preview (clientConnection net) st
                    return (views csNick idText cs)
    "disconnect" -> discoTime
    _         -> Nothing


-- | Respond to the TAB key being pressed. This can dispatch to a command
-- specific completion mode when relevant. Otherwise this will complete
-- input based on the users of the channel related to the current buffer.
tabCompletion :: Bool {- ^ reversed -} -> ClientState -> IO CommandResult
tabCompletion isReversed st =
  case snd $ clientLine st of
    '/':command -> executeCommand (Just isReversed) command st
    _           -> nickTabCompletion isReversed st

-- | Treat the current text input as a chat message and send it.
executeChat :: String -> ClientState -> IO CommandResult
executeChat msg st =
  case view clientFocus st of
    ChannelFocus network channel
      | Just !cs <- preview (clientConnection network) st ->
          do now <- getZonedTime
             let msgTxt = Text.pack $ takeWhile (/='\n') msg
                 tgtTxt = idText channel

             (st1,allow) <- clientPark st $ \ptr ->
                do let aes = view (clientExtensions . esActive) st
                   chatExtension ptr network tgtTxt msgTxt aes

             when allow (sendMsg cs (ircPrivmsg tgtTxt msgTxt))

             let myNick = UserInfo (view csNick cs) "" ""
                 entry = ClientMessage
                   { _msgTime    = now
                   , _msgNetwork = network
                   , _msgBody    = IrcBody (Privmsg myNick channel msgTxt) }
             commandSuccess $! recordChannelMessage network channel entry st1

    _ -> commandFailureMsg "cannot send chat messages to this window" st


-- | Parse and execute the given command. When the first argument is Nothing
-- the command is executed, otherwise the first argument is the cursor
-- position for tab-completion
executeCommand :: Maybe Bool -> String -> ClientState -> IO CommandResult

executeCommand (Just isReversed) _ st
  | Just st' <- commandNameCompletion isReversed st = commandSuccess st'

executeCommand tabCompleteReversed str st =
  let (cmd, rest) = break (==' ') str
      cmdTxt      = Text.toLower (Text.pack cmd)

      finish spec exec tab =
        case tabCompleteReversed of
          Just isReversed -> tab isReversed st rest
          Nothing ->
            case parseArguments spec rest of
              Nothing -> commandFailureMsg "bad command arguments" st
              Just arg -> exec st arg
  in
  case recognize cmdTxt commands of

    Exact Command{cmdImplementation=impl, cmdArgumentSpec=argSpec} ->
      case impl of
        ClientCommand exec tab ->
          finish argSpec exec tab

        NetworkCommand exec tab
          | Just network <- views clientFocus focusNetwork st
          , Just cs      <- preview (clientConnection network) st ->
              finish argSpec (exec cs) (\x -> tab x cs)
          | otherwise -> commandFailureMsg "command requires focused network" st

        ChannelCommand exec tab
          | ChannelFocus network channelId <- view clientFocus st
          , Just cs <- preview (clientConnection network) st
          , isChannelIdentifier cs channelId ->
              finish argSpec (exec channelId cs) (\x -> tab x channelId cs)
          | otherwise -> commandFailureMsg "command requires focused channel" st

        ChatCommand exec tab
          | ChannelFocus network channelId <- view clientFocus st
          , Just cs <- preview (clientConnection network) st ->
              finish argSpec (exec channelId cs) (\x -> tab x channelId cs)
          | otherwise -> commandFailureMsg "command requires focused chat window" st

    _ -> case tabCompleteReversed of
           Just isReversed -> nickTabCompletion isReversed st
           Nothing         -> commandFailureMsg "unknown command" st


-- | Expands each alias to have its own copy of the command callbacks
expandAliases :: [Command] -> [(Text,Command)]
expandAliases xs =
  [ (name, cmd) | cmd <- xs, name <- toList (cmdNames cmd) ]


-- | Map of built-in client commands to their implementations, tab completion
-- logic, and argument structures.
commands :: Recognizer Command
commands = fromCommands (expandAliases (concatMap cmdSectionCmds commandsList))


-- | Raw list of commands in the order used for @/help@
commandsList :: [CommandSection]
commandsList =

  ------------------------------------------------------------------------
  [ CommandSection "Client commands"
  ------------------------------------------------------------------------

  [ Command
      (pure "exit")
      NoArg
      "Exit the client immediately.\n"
    $ ClientCommand cmdExit noClientTab

  , Command
      (pure "reload")
      (OptTokenArg "filename" NoArg)
      "Reload the client configuration file.\n\
      \\n\
      \If \^Bfilename\^B is provided it will be used to reload.\n\
      \Otherwise the previously loaded configuration file will be reloaded.\n"
    $ ClientCommand cmdReload tabReload

  , Command
      (pure "extension")
      (ReqTokenArg "extension" (RemainingArg "arguments"))
      "Calls the process_command callback of the given extension.\n\
      \\n\
      \\^Bextension\^B should be the name of the loaded extension.\n"
    $ ClientCommand cmdExtension simpleClientTab

  , Command
      (pure "palette")
      NoArg
      "Show the current palette settings and a color chart to help pick new colors.\n"
    $ ClientCommand cmdPalette noClientTab

  , Command
      (pure "digraphs")
      NoArg
      "Show the table of digraphs.\n\
      \\n\
      \To enter a digraph type the two-character entry and press M-k.\n"
    $ ClientCommand cmdDigraphs noClientTab

  , Command
      (pure "keymap")
      NoArg
      "Show the key binding map.\n\
      \\n\
      \Key bindings can be changed in configuration file. See `glirc2 --config-format`.\n"
    $ ClientCommand cmdKeyMap noClientTab

  , Command
      (pure "rtsstats")
      NoArg
      "Show the GHC RTS statistics.\n"
    $ ClientCommand cmdRtsStats noClientTab

  , Command
      (pure "exec")
      (RemainingArg "arguments")
      "Execute a command synchnonously sending the to a configuration destination.\n\
      \\n\
      \\^Barguments\^B: [-n network] [-c channel] [-i input] command [command arguments...]\n\
      \\n\
      \When \^Binput\^B is specified it is sent to the stdin.\n\
      \\n\
      \When neither \^Bnetwork\^B nor \^Bchannel\^B are specified output goes to client window (*).\n\
      \When \^Bnetwork\^B is specified output is sent as raw IRC traffic to the network.\n\
      \When \^Bchannel\^B is specified output is sent as chat to the given channel on the current network.\n\
      \When \^Bnetwork\^B and \^Bchannel\^B are specified output is sent as chat to the given channel on the given network.\n\
      \\n\
      \\^Barguments\^B is divided on spaces into words before being processed\
      \ by getopt. Use Haskell string literal syntax to create arguments with\
      \ escaped characters and spaces inside.\n\
      \\n"
    $ ClientCommand cmdExec simpleClientTab

  , Command
      (pure "url")
      (OptTokenArg "number" NoArg)
      "Open a URL seen in chat.\n\
      \\n\
      \The URL is opened using the executable configured under \^Burl-opener\^B.\n\
      \\n\
      \When this command is active in the textbox, chat messages are filtered to only show ones with URLs.\n\
      \\n\
      \When \^Bnumber\^B is omitted it defaults to \^B1\^B. The number selects the URL to open counting back from the most recent.\n"
    $ ClientCommand cmdUrl noClientTab

  , Command
      (pure "help")
      (OptTokenArg "command" NoArg)
      "Show command documentation.\n\
      \\n\
      \When \^Bcommand\^B is omitted a list of all commands is displayed.\n\
      \When \^Bcommand\^B is specified detailed help for that command is shown.\n"
    $ ClientCommand cmdHelp tabHelp

  ------------------------------------------------------------------------
  ] , CommandSection "View toggles"
  ------------------------------------------------------------------------

  [ Command
      (pure "toggle-detail")
      NoArg
      "Toggle detailed message view.\n"
    $ ClientCommand cmdToggleDetail noClientTab

  , Command
      (pure "toggle-activity-bar")
      NoArg
      "Toggle detailed detailed activity information in status bar.\n"
    $ ClientCommand cmdToggleActivityBar noClientTab

  , Command
      (pure "toggle-metadata")
      NoArg
      "Toggle visibility of metadata in chat windows.\n"
    $ ClientCommand cmdToggleMetadata noClientTab

  , Command
      (pure "toggle-layout")
      NoArg
      "Toggle multi-window layout mode.\n"
    $ ClientCommand cmdToggleLayout noClientTab

  ------------------------------------------------------------------------
  ] , CommandSection "Connection commands"
  ------------------------------------------------------------------------

  [ Command
      (pure "connect")
      (ReqTokenArg "network" NoArg)
      "Connect to \^Bnetwork\^B by name.\n\
      \\n\
      \If no name is configured the hostname is the 'name'.\n"
    $ ClientCommand cmdConnect tabConnect

  , Command
      (pure "reconnect")
      NoArg
      "Reconnect to the current network.\n"
    $ ClientCommand cmdReconnect noClientTab

  , Command
      (pure "disconnect")
      NoArg
      "Immediately terminate the current network connection.\n\
      \\n\
      \See also: /quit /exit\n"
    $ NetworkCommand cmdDisconnect noNetworkTab

  , Command
      (pure "quit")
      (RemainingArg "reason")
      "Gracefully disconnect the current network connection.\n\
      \\n\
      \\^Breason\^B: optional quit reason\n\
      \\n\
      \See also: /disconnect /exit\n"
    $ NetworkCommand cmdQuit   simpleNetworkTab

  ------------------------------------------------------------------------
  ] , CommandSection "Window management"
  ------------------------------------------------------------------------

  [ Command
      (pure "focus")
      (ReqTokenArg "network" (OptTokenArg "target" NoArg))
      "Change the focused window.\n\
      \\n\
      \When only \^Bnetwork\^B is specified this switches to the network status window.\n\
      \When \^Bnetwork\^B and \^Btarget\^B are specified this switches to that chat window.\n\
      \\n\
      \Nickname and channels can be specified in the \^Btarget\^B parameter.\n\
      \See also: /query (aliased /c /channel) to switch to a target on the current network.\n"
    $ ClientCommand cmdFocus tabFocus

  , Command
      ("query" :| ["c", "channel"])
      (ReqTokenArg "focus" NoArg)
      "\^BParameters:\^B\n\
      \\n\
      \    focuses: Focus name\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    This command sets the current window focus. When\n\
      \    no network is specified, the current network will\n\
      \    be used.\n\
      \\n\
      \    Client:  *\n\
      \    Network: \^_network\^_:\n\
      \    Channel: \^_#channel\^_\n\
      \    Channel: \^_network\^_:\^_#channel\^_\n\
      \    User:    \^_nick\^_\n\
      \    User:    \^_network\^_:\^_nick\^_\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /c fn:#haskell\n\
      \    /c #haskell\n\
      \    /c fn:\n\
      \    /c *:\n\
      \\n\
      \\^BSee also:\^B focus\n"
    $ ClientCommand cmdQuery tabQuery

  , Command
      (pure "clear")
      (OptTokenArg "network" (OptTokenArg "channel" NoArg))
      "Clear a window.\n\
      \\n\
      \If no arguments are provided the current window is cleared.\n\
      \If \^Bnetwork\^B is provided the that network window is cleared.\n\
      \If \^Bnetwork\^B and \^Bchannel\^B are provided that chat window is cleared.\n\
      \If \^Bnetwork\^B is provided and \^Bchannel\^B is \^B*\^O all windows for that network are cleared.\n\
      \\n\
      \If a window is cleared and no longer active that window will be removed from the client.\n"
    $ ClientCommand cmdClear tabFocus

  , Command
      (pure "windows")
      (OptTokenArg "kind" NoArg)
      "Show a list of all windows with an optional argument to limit the kinds of windows listed.\n\
      \\n\
      \\^Bkind\^O: one of \^Bnetworks\^O, \^Bchannels\^O, \^Busers\^O\n\
      \\n"
    $ ClientCommand cmdWindows tabWindows

  , Command
      (pure "splits")
      (RemainingArg "focuses")
      "\^BParameters:\^B\n\
      \\n\
      \    focuses: List of focus names\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    This command sents the set of focuses that will always\n\
      \    be visible, even when unfocused. When the client is focused\n\
      \    to an active network, the network can be omitted when\n\
      \    specifying a focus. If no focuses are listed, they will\n\
      \    all be cleared.\n\
      \\n\
      \    Client:  *\n\
      \    Network: \^_network\^_:\n\
      \    Channel: \^_#channel\^_\n\
      \    Channel: \^_network\^_:\^_#channel\^_\n\
      \    User:    \^_nick\^_\n\
      \    User:    \^_network\^_:\^_nick\^_\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /splits * fn:#haskell fn:chanserv\n\
      \    /splits #haskell #haskell-lens nickserv\n\
      \    /splits\n\
      \\n\
      \\^BSee also:\^B splits+, splits-\n"
    $ ClientCommand cmdSplits tabSplits

  , Command
      (pure "splits+")
      (RemainingArg "focuses")
      "Add windows to the splits list. Omit the list of focuses to add the\
      \ current window.\n\
      \\n\
      \\^Bfocuses\^B: space delimited list of focus names.\n\
      \\n\
      \Client:  *\n\
      \Network: \^BNETWORK\^B\n\
      \Channel: \^BNETWORK\^B:\^B#CHANNEL\^B\n\
      \User:    \^BNETWORK\^B:\^BNICK\^B\n\
      \\n\
      \If the network part is omitted, the current network will be used.\n"
    $ ClientCommand cmdSplitsAdd tabSplits

  , Command
      (pure "splits-")
      (RemainingArg "focuses")
      "Remove windows from the splits list. Omit the list of focuses to\
      \ remove the current window.\n\
      \\n\
      \\^Bfocuses\^B: space delimited list of focus names.\n\
      \\n\
      \Client:  *\n\
      \Network: \^BNETWORK\^B\n\
      \Channel: \^BNETWORK\^B:\^B#CHANNEL\^B\n\
      \User:    \^BNETWORK\^B:\^BNICK\^B\n\
      \\n\
      \If the network part is omitted, the current network will be used.\n"
    $ ClientCommand cmdSplitsDel tabActiveSplits

  , Command
      (pure "ignore")
      (RemainingArg "nicks")
      "Toggle the soft-ignore on each of the space-delimited given nicknames.\n"
    $ ClientCommand cmdIgnore simpleClientTab

  , Command
      (pure "grep")
      (RemainingArg "regular-expression")
      "Set the persistent regular expression.\n\
      \\n\
      \Clear the regular expression by calling this without an argument.\n\
      \\n\
      \\^B/grep\^O is case-sensitive.\n\
      \\^B/grepi\^O is case-insensitive.\n"
    $ ClientCommand (cmdGrep True) simpleClientTab

  , Command
      (pure "grepi")
      (RemainingArg "regular-expression")
      "Set the persistent regular expression.\n\
      \\n\
      \Clear the regular expression by calling this without an argument.\n\
      \\n\
      \\^B/grep\^O is case-sensitive.\n\
      \\^B/grepi\^O is case-insensitive.\n"
    $ ClientCommand (cmdGrep False) simpleClientTab

  , Command
      (pure "mentions")
      NoArg
      "Show a list of all message that were highlighted as important.\n"
    $ ClientCommand cmdMentions noClientTab

  ------------------------------------------------------------------------
  ] , CommandSection "IRC commands"
  ------------------------------------------------------------------------

  [ Command
      ("join" :| ["j"])
      (ReqTokenArg "channels" (OptTokenArg "keys" NoArg))
      "\^BParameters:\^B\n\
      \\n\
      \    channels: Comma-separated list of channels\n\
      \    keys:     Comma-separated list of keys\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Join the given channels. When keys are provided, they should\n\
      \    occur in the same order as the channels.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /join #friends\n\
      \    /join #secret thekey\n\
      \    /join #secret1,#secret2 key1,key2\n\
      \\n\
      \\^BSee also:\^B channel, clear, part\n"
    $ NetworkCommand cmdJoin simpleNetworkTab

  , Command
      (pure "part")
      (RemainingArg "reason")
      "\^BParameters:\^B\n\
      \\n\
      \    reason: Optional message sent to channel as part reason\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Part from the current channel.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /part\n\
      \    /part It's not me, it's you\n\
      \\n\
      \\^BSee also:\^B clear, join, quit\n"
    $ ChannelCommand cmdPart simpleChannelTab

  , Command
      (pure "msg")
      (ReqTokenArg "target" (RemainingArg "message"))
      "\^BParameters:\^B\n\
      \\n\
      \    target:  Comma-separated list of nicknames and channels\n\
      \    message: Formatted message body\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Send a chat message to a user or a channel. On servers\n\
      \    with STATUSMSG support, the channel name can be prefixed\n\
      \    with a sigil to restrict the recipients to those with the\n\
      \    given mode.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /msg buddy I'm sending you a message.\n\
      \    /msg #friends This message is for the whole channel.\n\
      \    /msg him,her I'm chatting with two people.\n\
      \    /msg @#users This message is only for ops!\n\
      \\n\
      \\^BSee also:\^B notice, me, say\n"
    $ NetworkCommand cmdMsg simpleNetworkTab

  , Command
      (pure "me")
      (RemainingArg "message")
      "\^BParameters:\^B\n\
      \\n\
      \    message: Body of action message\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Sends an action message to the currently focused channel.\n\
      \    Most clients will render these messages prefixed with\n\
      \    only your nickname as though describing an action.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /me shrugs\n\
      \\n\
      \\^BSee also:\^B notice, msg, say\n"
    $ ChatCommand cmdMe simpleChannelTab

  , Command
      (pure "say")
      (RemainingArg "message")
      "\^BParameters:\^B\n\
      \\n\
      \    message: Body of message\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Send a message to the current chat window.  This can be useful\n\
      \    for sending a chat message with a leading '/' to the current\n\
      \    chat window.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /say /help is the right place to start!\n\
      \\n\
      \\^BSee also:\^B notice, me, msg\n"
    $ ChatCommand cmdSay simpleChannelTab

  , Command
      (pure "notice")
      (ReqTokenArg "target" (RemainingArg "message"))
      "\^BParameters:\^B\n\
      \\n\
      \    target:  Comma-separated list of nicknames and channels\n\
      \    message: Formatted message body\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Send a chat notice to a user or a channel. On servers\n\
      \    with STATUSMSG support, the channel name can be prefixed\n\
      \    with a sigil to restrict the recipients to those with the\n\
      \    given mode. Notice messages were originally intended to be\n\
      \    used by bots. Different clients will render these in different\n\
      \    ways.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /notice buddy I'm sending you a message.\n\
      \    /notice #friends This message is for the whole channel.\n\
      \    /notice him,her I'm chatting with two people.\n\
      \    /notice @#users This message is only for ops!\n\
      \\n\
      \\^BSee also:\^B me, msg, say\n"
    $ NetworkCommand cmdNotice simpleNetworkTab

  , Command
      (pure "ctcp")
      (ReqTokenArg "target" (ReqTokenArg "command" (RemainingArg "arguments")))
      "\^BParameters:\^B\n\
      \\n\
      \    target:    Comma-separated list of nicknames and channels\n\
      \    command:   CTCP command name\n\
      \    arguments: CTCP command arguments\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Client-to-client protocol (CTCP) commands can be used\n\
      \    to query information from another user's client application\n\
      \    directly. Common CTCP commands include: ACTION, PING, VERSION,\n\
      \    USERINFO, CLIENTINFO, and TIME. glirc does not automatically\n\
      \    respond to CTCP commands.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /ctcp myfriend VERSION\n\
      \    /ctcp myfriend CLIENTINFO\n"
    $ NetworkCommand cmdCtcp simpleNetworkTab

  , Command
      (pure "nick")
      (ReqTokenArg "nick" NoArg)
      "\^BParameters:\^B\n\
      \\n\
      \    nick: New nickname\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Change your nickname on the currently focused server.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /nick guest123\n\
      \    /nick better_nick\n"
    $ NetworkCommand cmdNick simpleNetworkTab

  , Command
      (pure "away")
      (RemainingArg "message")
      "\^BParameters:\^B\n\
      \\n\
      \    message: Optional away message\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Change your nickname on the currently focused server.\n\
      \    Omit the message parameter to clear your away status.\n\
      \    The away message is only used by the server to update\n\
      \    status in /whois and to provide automated responses.\n\
      \    It is not used by this client directly.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /away\n\
      \    /away Out getting some sun\n"
    $ NetworkCommand cmdAway simpleNetworkTab

  , Command
      ("users" :| ["names"])
      NoArg
      "\^BDescription:\^B\n\
      \\n\
      \    Show the user list for the current channel.\n\
      \    Detailed view (default key F2) shows full hostmask.\n\
      \    Hostmasks can be populated with /who #channel.\n\
      \    Press ESC to exit the userlist.\n\
      \\n\
      \\^BSee also:\^B channelinfo, masks\n"
    $ ChannelCommand cmdUsers  noChannelTab

  , Command
      (pure "channelinfo")
      NoArg
      "\^BDescription:\^B\n\
      \\n\
      \    Show information about the current channel.\n\
      \    Press ESC to exit the channel info window.\n\
      \\n\
      \\^BSee also:\^B masks, users\n"
    $ ChannelCommand cmdChannelInfo noChannelTab

  , Command
      (pure "quote")
      (RemainingArg "raw IRC command")
      "Send a raw IRC command.\n"
    $ NetworkCommand cmdQuote  simpleNetworkTab

  ------------------------------------------------------------------------
  ] , CommandSection "IRC queries"
  ------------------------------------------------------------------------

  [ Command
      (pure "who")
      (RemainingArg "arguments")
      "Send WHO query to server with given arguments.\n"
    $ NetworkCommand cmdWho simpleNetworkTab

  , Command
      (pure "whois")
      (RemainingArg "arguments")
      "Send WHOIS query to server with given arguments.\n"
    $ NetworkCommand cmdWhois simpleNetworkTab

  , Command
      (pure "whowas")
      (RemainingArg "arguments")
      "Send WHOWAS query to server with given arguments.\n"
    $ NetworkCommand cmdWhowas simpleNetworkTab

  , Command
      (pure "ison")
      (RemainingArg "arguments")
      "Send ISON query to server with given arguments.\n"
    $ NetworkCommand cmdIson   simpleNetworkTab

  , Command
      (pure "userhost")
      (RemainingArg "arguments")
      "Send USERHOST query to server with given arguments.\n"
    $ NetworkCommand cmdUserhost simpleNetworkTab

  , Command
      (pure "time")
      (OptTokenArg "servername" NoArg)
      "Send TIME query to server with given arguments.\n"
    $ NetworkCommand cmdTime simpleNetworkTab

  , Command
      (pure "stats")
      (RemainingArg "arguments")
      "Send STATS query to server with given arguments.\n"
    $ NetworkCommand cmdStats simpleNetworkTab

  , Command
      (pure "lusers")
      (OptTokenArg "mask" (OptTokenArg "servername" NoArg))
      "Send LUSERS query to server with given arguments.\n"
    $ NetworkCommand cmdLusers simpleNetworkTab

  , Command
      (pure "motd") (OptTokenArg "servername" NoArg)
      "Send MOTD query to server.\n"
    $ NetworkCommand cmdMotd simpleNetworkTab

  , Command
      (pure "admin") (OptTokenArg "servername" NoArg)
      "Send ADMIN query to server.\n"
    $ NetworkCommand cmdAdmin simpleNetworkTab

  , Command
      (pure "rules") (OptTokenArg "servername" NoArg)
      "Send RULES query to server.\n"
    $ NetworkCommand cmdRules simpleNetworkTab

  , Command
      (pure "info") NoArg
      "Send INFO query to server.\n"
    $ NetworkCommand cmdInfo noNetworkTab

  , Command
      (pure "list") (RemainingArg "arguments")
      "Send LIST query to server.\n"
    $ NetworkCommand cmdList simpleNetworkTab

  , Command
      (pure "version") (OptTokenArg "servername" NoArg)
      "Send VERSION query to server.\n"
    $ NetworkCommand cmdVersion simpleNetworkTab

  ------------------------------------------------------------------------
  ] , CommandSection "IRC channel management"
  ------------------------------------------------------------------------

  [ Command
      (pure "mode")
      (RemainingArg "modes and parameters")
      "Sets IRC modes.\n\
      \\n\
      \Examples:\n\
      \Setting a ban:           /mode +b *!*@hostname\n\
      \Removing a quiet:        /mode -q *!*@hostname\n\
      \Voicing two users:       /mode +vv user1 user2\n\
      \Demoting an op to voice: /mode +v-o user1 user1\n\
      \\n\
      \When executed in a network window, mode changes are applied to your user.\n\
      \When executed in a channel window, mode changes are applied to the channel.\n\
      \\n\
      \This command has parameter sensitive tab-completion.\n"
    $ NetworkCommand cmdMode   tabMode

  , Command
      (pure "masks")
      (ReqTokenArg "mode" NoArg)
      "Show mask lists for current channel.\n\
      \\n\
      \Common \^Bmode\^B values:\n\
      \\^Bb\^B: bans\n\
      \\^Bq\^B: quiets\n\
      \\^BI\^B: invite exemptions (op view only)\n\
      \\^Be\^B: ban exemption (op view only)s\n\
      \\n\
      \To populate the mask lists for the first time use: /mode \^Bmode\^B\n"
    $ ChannelCommand cmdMasks noChannelTab

  , Command
      (pure "invite")
      (ReqTokenArg "nick" NoArg)
      "Invite a user to the current channel.\n"
    $ ChannelCommand cmdInvite simpleChannelTab

  , Command
      (pure "topic")
      (RemainingArg "message")
      "Set the topic on the current channel.\n\
      \\n\
      \Tab-completion with no \^Bmessage\^B specified will load the current topic for editing.\n"
    $ ChannelCommand cmdTopic tabTopic

  , Command
      (pure "kick")
      (ReqTokenArg "nick" (RemainingArg "reason"))
      "Kick a user from the current channel.\n\
      \\n\
      \See also: /kickban /remove\n"
    $ ChannelCommand cmdKick   simpleChannelTab

  , Command
      (pure "kickban")
      (ReqTokenArg "nick" (RemainingArg "reason"))
      "Ban and kick a user from the current channel.\n\
      \\n\
      \Users are banned by hostname match.\n\
      \See also: /kick /remove\n"
    $ ChannelCommand cmdKickBan simpleChannelTab

  , Command
      (pure "remove")
      (ReqTokenArg "nick" (RemainingArg "reason"))
      "Remove a user from the current channel.\n\
      \\n\
      \Remove works like /kick except it results in a PART.\n\
      \See also: /kick /kickban\n"
    $ ChannelCommand cmdRemove simpleChannelTab

  , Command
      (pure "knock")
      (ReqTokenArg "channel" (RemainingArg "message"))
      "Request entry to an invite-only channel.\n"
    $ NetworkCommand cmdKnock simpleNetworkTab

  ------------------------------------------------------------------------
  ] , CommandSection "ZNC Support"
  ------------------------------------------------------------------------

  [ Command
      (pure "znc")
      (RemainingArg "arguments")
      "Send command directly to ZNC.\n\
      \\n\
      \The advantage of this over /msg is that responses are not broadcast to call clients.\n"
    $ NetworkCommand cmdZnc simpleNetworkTab

  , Command
      (pure "znc-playback")
      (OptTokenArg "time" (OptTokenArg "date" NoArg))
      "Request playback from the ZNC 'playback' module.\n\
      \\n\
      \\^Btime\^B determines the time to playback since.\n\
      \\^Bdate\^B determines the date to playback since.\n\
      \\n\
      \When both \^Btime\^B and \^Bdate\^B are omitted, all playback is requested.\n\
      \When both \^Bdate\^B is omitted it is defaulted the most recent date in the past that makes sense.\n\
      \\n\
      \Time format: HOURS:MINUTES (example: 7:00)\n\
      \Date format: YEAR-MONTH-DAY (example: 2016-06-16)\n\
      \\n\
      \Note that the playback module is not installed in ZNC by default!\n"
    $ NetworkCommand cmdZncPlayback noNetworkTab

  ] , CommandSection "Network operator commands"

  [ Command
      (pure "oper")
      (ReqTokenArg "user" (ReqTokenArg "password" NoArg))
      "Authenticate as a server operator.\n"
    $ NetworkCommand cmdOper noNetworkTab

  , Command
      (pure "kill")
      (ReqTokenArg "client" (RemainingArg "reason"))
      "Kill a client connection to the server.\n"
    $ NetworkCommand cmdKill simpleNetworkTab

  , Command
      (pure "map")
      NoArg
      "Display network map.\n"
    $ NetworkCommand cmdMap simpleNetworkTab

  , Command
      (pure "links")
      (RemainingArg "arguments")
      "Send LINKS query to server with given arguments.\n"
    $ NetworkCommand cmdLinks simpleNetworkTab

  ]]

-- | Provides no tab completion for client commands
noClientTab :: Bool -> ClientCommand String
noClientTab _ st _ = commandFailure st

-- | Provides no tab completion for network commands
noNetworkTab :: Bool -> NetworkCommand String
noNetworkTab _ _ st _ = commandFailure st

-- | Provides no tab completion for channel commands
noChannelTab :: Bool -> ChannelCommand String
noChannelTab _ _ _ st _ = commandFailure st

-- | Provides nickname based tab completion for client commands
simpleClientTab :: Bool -> ClientCommand String
simpleClientTab isReversed st _ =
  nickTabCompletion isReversed st

-- | Provides nickname based tab completion for network commands
simpleNetworkTab :: Bool -> NetworkCommand String
simpleNetworkTab isReversed _ st _ =
  nickTabCompletion isReversed st

-- | Provides nickname based tab completion for channel commands
simpleChannelTab :: Bool -> ChannelCommand String
simpleChannelTab isReversed _ _ st _ =
  nickTabCompletion isReversed st

-- | Implementation of @/exit@ command.
cmdExit :: ClientCommand ()
cmdExit st _ = return (CommandQuit st)


cmdToggleDetail :: ClientCommand ()
cmdToggleDetail st _ = commandSuccess (over clientDetailView not st)

cmdToggleActivityBar :: ClientCommand ()
cmdToggleActivityBar st _ = commandSuccess (over clientActivityBar not st)

cmdToggleMetadata :: ClientCommand ()
cmdToggleMetadata st _ = commandSuccess (clientToggleHideMeta st)

cmdToggleLayout :: ClientCommand ()
cmdToggleLayout st _ = commandSuccess (set clientScroll 0 (over clientLayout aux st))
  where
    aux OneColumn = TwoColumn
    aux TwoColumn = OneColumn

-- | When used on a channel that the user is currently
-- joined to this command will clear the messages but
-- preserve the window. When used on a window that the
-- user is not joined to this command will delete the window.
cmdClear :: ClientCommand (Maybe (String, Maybe (String, ())))
cmdClear st args =
  case args of
    Nothing                 -> clearFocus (view clientFocus st)
    Just ("*", Nothing)     -> clearFocus Unfocused
    Just (network, Nothing) -> clearFocus (NetworkFocus (Text.pack network))
    Just (network, Just ("*", _)) -> clearNetworkWindows network
    Just (network, Just (channel, _)) ->
        clearFocus (ChannelFocus (Text.pack network) (mkId (Text.pack channel)))
  where
    clearNetworkWindows network
      = commandSuccess
      $ foldl' (flip clearFocus1) st
      $ filter (\x -> focusNetwork x == Just (Text.pack network))
      $ views clientWindows Map.keys st

    clearFocus focus = commandSuccess (clearFocus1 focus st)

    clearFocus1 focus st' = focusEffect (windowEffect st')
      where
        windowEffect
          | isActive  = setWindow (Just emptyWindow)
          | otherwise = setWindow Nothing

        focusEffect
          | not isActive && view clientFocus st' == focus =
                 if has (clientWindows . ix prev) st'
                 then changeFocus prev
                 else advanceFocus
          | otherwise = id
          where
            prev = view clientPrevFocus st

        setWindow = set (clientWindows . at focus)

        isActive =
          case focus of
            Unfocused                    -> False
            NetworkFocus network         -> has (clientConnection network) st'
            ChannelFocus network channel -> has (clientConnection network
                                                .csChannels . ix channel) st'

-- | Implementation of @/quote@. Parses arguments as a raw IRC command and
-- sends to the current network.
cmdQuote :: NetworkCommand String
cmdQuote cs st rest =
  case parseRawIrcMsg (Text.pack rest) of
    Nothing  -> commandFailureMsg "failed to parse raw IRC command" st
    Just raw ->
      do sendMsg cs raw
         commandSuccess st

-- | Implementation of @/me@
cmdMe :: ChannelCommand String
cmdMe channelId cs st rest =
  do now <- getZonedTime
     let actionTxt = Text.pack ("\^AACTION " ++ rest ++ "\^A")
         !myNick = UserInfo (view csNick cs) "" ""
         network = view csNetwork cs
         entry = ClientMessage
                    { _msgTime = now
                    , _msgNetwork = network
                    , _msgBody = IrcBody (Ctcp myNick channelId "ACTION" (Text.pack rest))
                    }
     sendMsg cs (ircPrivmsg (idText channelId) actionTxt)
     commandSuccess
       $! recordChannelMessage network channelId entry st

-- | Implementation of @/ctcp@
cmdCtcp :: NetworkCommand (String, (String, String))
cmdCtcp cs st (target, (cmd, args)) =
  do let cmdTxt = Text.toUpper (Text.pack cmd)
         argTxt = Text.pack args
         tgtTxt = Text.pack target

     sendMsg cs (ircPrivmsg tgtTxt ("\^A" <> cmdTxt <> " " <> argTxt <> "\^A"))
     chatCommand
        (\src tgt -> Ctcp src tgt cmdTxt argTxt)
        tgtTxt cs st

-- | Implementation of @/notice@
cmdNotice :: NetworkCommand (String, String)
cmdNotice cs st (target, rest)
  | null rest = commandFailureMsg "empty message" st
  | otherwise =
      do let restTxt = Text.pack rest
             tgtTxt = Text.pack target

         sendMsg cs (ircNotice tgtTxt restTxt)
         chatCommand
            (\src tgt -> Notice src tgt restTxt)
            tgtTxt cs st

-- | Implementation of @/msg@
cmdMsg :: NetworkCommand (String, String)
cmdMsg cs st (target, rest)
  | null rest = commandFailureMsg "empty message" st
  | otherwise =
      do let restTxt = Text.pack rest
             tgtTxt = Text.pack target

         sendMsg cs (ircPrivmsg tgtTxt restTxt)
         chatCommand
            (\src tgt -> Privmsg src tgt restTxt)
            tgtTxt cs st

-- | Common logic for @/msg@ and @/notice@
chatCommand ::
  (UserInfo -> Identifier -> IrcMsg) ->
  Text {- ^ target  -} ->
  NetworkState         ->
  ClientState          ->
  IO CommandResult
chatCommand mkmsg target cs st =
  commandSuccess =<< chatCommand' mkmsg target cs st

-- | Common logic for @/msg@ and @/notice@ returning the client state
chatCommand' ::
  (UserInfo -> Identifier -> IrcMsg) ->
  Text {- ^ target  -} ->
  NetworkState         ->
  ClientState          ->
  IO ClientState
chatCommand' con targetsTxt cs st =
  do now <- getZonedTime
     let targetTxts = Text.split (==',') targetsTxt
         targetIds  = mkId <$> targetTxts
         !myNick = UserInfo (view csNick cs) "" ""
         network = view csNetwork cs
         entries = [ (targetId,
                          ClientMessage
                          { _msgTime = now
                          , _msgNetwork = network
                          , _msgBody = IrcBody (con myNick targetId)
                          })
                       | targetId <- targetIds ]

     return $! foldl' (\acc (targetId, entry) ->
                        recordChannelMessage network targetId entry acc)
                      st
                      entries


cmdConnect :: ClientCommand (String, ())
cmdConnect st (networkStr, _) =
  do -- abort any existing connection before connecting
     let network = Text.pack networkStr
     st' <- addConnection 0 Nothing network =<< abortNetwork network st
     commandSuccess
       $ changeFocus (NetworkFocus network) st'

cmdFocus :: ClientCommand (String, Maybe (String, ()))
cmdFocus st (network, mbChannel)
  | network == "*" = commandSuccess (changeFocus Unfocused st)
  | otherwise =
     case mbChannel of
       Nothing ->
         let focus = NetworkFocus (Text.pack network) in
         commandSuccess (changeFocus focus st)
       Just (channel,_) ->
         let focus = ChannelFocus (Text.pack network) (mkId (Text.pack channel)) in
         commandSuccess
           $ changeFocus focus st


tabWindows :: Bool -> ClientCommand String
tabWindows isReversed st _ =
  simpleTabCompletion plainWordCompleteMode [] completions isReversed st
  where
    completions = ["networks","channels","users"] :: [Text]


-- | Implementation of @/windows@ command. Set subfocus to Windows.
cmdWindows :: ClientCommand (Maybe (String, ()))
cmdWindows st arg =
  case arg of
    Nothing             -> success AllWindows
    Just ("networks",_) -> success NetworkWindows
    Just ("channels",_) -> success ChannelWindows
    Just ("users"   ,_) -> success UserWindows
    _                   -> commandFailureMsg errmsg st
  where
    errmsg = "/windows expected networks, channels, or users"
    success x =
      commandSuccess (changeSubfocus (FocusWindows x) st)

-- | Implementation of @/mentions@ command. Set subfocus to Mentions.
cmdMentions :: ClientCommand ()
cmdMentions st _ = commandSuccess (changeSubfocus FocusMentions st)

-- | Implementation of @/palette@ command. Set subfocus to Palette.
cmdPalette :: ClientCommand ()
cmdPalette st _ = commandSuccess (changeSubfocus FocusPalette st)

-- | Implementation of @/digraphs@ command. Set subfocus to Digraphs.
cmdDigraphs :: ClientCommand ()
cmdDigraphs st _ = commandSuccess (changeSubfocus FocusDigraphs st)

-- | Implementation of @/keymap@ command. Set subfocus to Keymap.
cmdKeyMap :: ClientCommand ()
cmdKeyMap st _ = commandSuccess (changeSubfocus FocusKeyMap st)

-- | Implementation of @/rtsstats@ command. Set subfocus to RtsStats.
-- Update cached rts stats in client state.
cmdRtsStats :: ClientCommand ()
cmdRtsStats st _ =
  do mb <- getStats
     case mb of
       Nothing -> commandFailureMsg "RTS statistics not available. (Use +RTS -T)" st
       Just{}  -> commandSuccess $ set clientRtsStats mb
                                 $ changeSubfocus FocusRtsStats st

-- | Implementation of @/help@ command. Set subfocus to Help.
cmdHelp :: ClientCommand (Maybe (String, ()))
cmdHelp st mb = commandSuccess (changeSubfocus focus st)
  where
    focus = FocusHelp (fmap (Text.pack . fst) mb)

-- | Tab completion for @/splits[+]@. When given no arguments this
-- populates the current list of splits, otherwise it tab completes
-- all of the currently available windows.
tabSplits :: Bool -> ClientCommand String
tabSplits isReversed st rest

  -- If no arguments, populate the current splits
  | all (' '==) rest =
     let cmd = unwords $ "/splits"
                       : map (Text.unpack . renderSplitFocus) currentExtras

         currentExtras = view clientExtraFocus st
         newline = Edit.endLine cmd
     in commandSuccess (set (clientTextBox . Edit.line) newline st)

  -- Tab complete the available windows. Accepts either fully qualified
  -- window names or current network names without the ':'
  | otherwise =
     let completions = currentNet <> allWindows
         allWindows  = renderSplitFocus <$> views clientWindows Map.keys st
         currentNet  = case views clientFocus focusNetwork st of
                         Just net -> idText <$> channelWindowsOnNetwork net st
                         Nothing  -> []
     in simpleTabCompletion plainWordCompleteMode [] completions isReversed st


-- | Tab completion for @/splits-@. This completes only from the list of active
-- entries in the splits list.
tabActiveSplits :: Bool -> ClientCommand String
tabActiveSplits isReversed st _ =
  simpleTabCompletion plainWordCompleteMode [] completions isReversed st
  where
    completions = currentNetSplits <> currentSplits
    currentSplits = renderSplitFocus <$> view clientExtraFocus st
    currentNetSplits =
      [ idText chan
        | ChannelFocus net chan <- view clientExtraFocus st
        , views clientFocus focusNetwork st == Just net
        ]


withSplitFocuses ::
  ClientState                   ->
  String                        ->
  ([Focus] -> IO CommandResult) ->
  IO CommandResult
withSplitFocuses st str k =
  case mb of
    Nothing   -> commandFailureMsg "unable to parse arguments" st
    Just args -> k args
  where
    mb = traverse
           (parseFocus (views clientFocus focusNetwork st))
           (words str)

-- | Parses a single focus name given a default network.
--
-- The default is parameterized over an arbitrary 'Applicative'
-- instance so that if you know the network you can use 'Identity'
-- and if you might not, you can use 'Maybe'
parseFocus ::
  Applicative f =>
  f Text {- ^ default network    -} ->
  String {- ^ @[network:]target@ -} ->
  f Focus
parseFocus mbNet x =
  case break (==':') x of
    ("*","")     -> pure Unfocused
    (net,_:"")   -> pure (NetworkFocus (Text.pack net))
    (net,_:chan) -> pure (ChannelFocus (Text.pack net) (mkId (Text.pack chan)))
    (chan,"")    -> mbNet <&> \net ->
                    ChannelFocus net (mkId (Text.pack chan))

-- | Render a entry from splits back to the textual format.
renderSplitFocus :: Focus -> Text
renderSplitFocus Unfocused          = "*"
renderSplitFocus (NetworkFocus x)   = x <> ":"
renderSplitFocus (ChannelFocus x y) = x <> ":" <> idText y


-- | Implementation of @/splits@
cmdSplits :: ClientCommand String
cmdSplits st str =
  withSplitFocuses st str $ \args ->
    commandSuccess (setExtraFocus (nub args) st)


-- | Implementation of @/splits+@. When no focuses are provided
-- the current focus is used instead.
cmdSplitsAdd :: ClientCommand String
cmdSplitsAdd st str =
  withSplitFocuses st str $ \args ->
    let args'
          | null args = st ^.. clientFocus
          | otherwise = args
        extras = nub (args' ++ view clientExtraFocus st)

    in commandSuccess (setExtraFocus extras st)

-- | Implementation of @/splits-@. When no focuses are provided
-- the current focus is used instead.
cmdSplitsDel :: ClientCommand String
cmdSplitsDel st str =
  withSplitFocuses st str $ \args ->
    let args'
          | null args = st ^.. clientFocus
          | otherwise = args
        extras = view clientExtraFocus st \\ args'

    in commandSuccess (setExtraFocus extras st)


tabHelp :: Bool -> ClientCommand String
tabHelp isReversed st _ =
  simpleTabCompletion plainWordCompleteMode [] commandNames isReversed st
  where
    commandNames = fst <$> expandAliases (concatMap cmdSectionCmds commandsList)

simpleTabCompletion ::
  Prefix a =>
  WordCompletionMode {- ^ word completion mode -} ->
  [a]                {- ^ hints                -} ->
  [a]                {- ^ all completions      -} ->
  Bool               {- ^ reversed order       -} ->
  ClientState        {- ^ client state         -} ->
  IO CommandResult
simpleTabCompletion mode hints completions isReversed st =
  case traverseOf clientTextBox tryCompletion st of
    Nothing  -> commandFailure st
    Just st' -> commandSuccess st'
  where
    tryCompletion = wordComplete mode isReversed hints completions

-- | @/connect@ tab completes known server names
tabConnect :: Bool -> ClientCommand String
tabConnect isReversed st _ =
  simpleTabCompletion plainWordCompleteMode [] networks isReversed st
  where
    networks = views clientNetworkMap               HashMap.keys st
            ++ views (clientConfig . configServers) HashMap.keys st


-- | When tab completing the first parameter of the focus command
-- the current networks are used.
tabFocus :: Bool -> ClientCommand String
tabFocus isReversed st _ =
  simpleTabCompletion plainWordCompleteMode [] completions isReversed st
  where
    networks   = map mkId $ HashMap.keys $ view clientNetworkMap st
    params     = words $ uncurry take $ clientLine st

    completions =
      case params of
        [_cmd,_net]      -> networks
        [_cmd,net,_chan] -> channelWindowsOnNetwork (Text.pack net) st
        _                -> []

cmdWhois :: NetworkCommand String
cmdWhois cs st rest =
  do sendMsg cs (ircWhois (Text.pack <$> words rest))
     commandSuccess st

cmdWho :: NetworkCommand String
cmdWho cs st rest =
  do sendMsg cs (ircWho (Text.pack <$> words rest))
     commandSuccess st

cmdWhowas :: NetworkCommand String
cmdWhowas cs st rest =
  do sendMsg cs (ircWhowas (Text.pack <$> words rest))
     commandSuccess st

cmdIson :: NetworkCommand String
cmdIson cs st rest =
  do sendMsg cs (ircIson (Text.pack <$> words rest))
     commandSuccess st

cmdUserhost :: NetworkCommand String
cmdUserhost cs st rest =
  do sendMsg cs (ircUserhost (Text.pack <$> words rest))
     commandSuccess st

cmdStats :: NetworkCommand String
cmdStats cs st rest =
  do sendMsg cs (ircStats (Text.pack <$> words rest))
     commandSuccess st

cmdLusers :: NetworkCommand (Maybe (String, Maybe (String, ())))
cmdLusers cs st arg =
  do sendMsg cs $ ircLusers $ fmap Text.pack $
       case arg of
         Nothing                -> []
         Just (x, Nothing)      -> [x]
         Just (x, (Just (y,_))) -> [x,y]
     commandSuccess st

cmdMotd :: NetworkCommand (Maybe (String, ()))
cmdMotd cs st mbservername =
  do sendMsg cs $ ircMotd $ case mbservername of
                              Just (s,_) -> Text.pack s
                              Nothing    -> ""
     commandSuccess st

cmdAdmin :: NetworkCommand (Maybe (String, ()))
cmdAdmin cs st mbservername =
  do sendMsg cs $ ircAdmin $ case mbservername of
                              Just (s,_) -> Text.pack s
                              Nothing    -> ""
     commandSuccess st

cmdRules :: NetworkCommand (Maybe (String, ()))
cmdRules cs st mbservername =
  do sendMsg cs $ ircRules $
       case mbservername of
         Just (s,_) -> Text.pack s
         Nothing    -> ""
     commandSuccess st

cmdMap :: NetworkCommand ()
cmdMap cs st _ =
  do sendMsg cs ircMap
     commandSuccess st

cmdInfo :: NetworkCommand ()
cmdInfo cs st _ =
  do sendMsg cs ircInfo
     commandSuccess st

cmdVersion :: NetworkCommand (Maybe (String, ()))
cmdVersion cs st mbservername =
  do sendMsg cs $ ircVersion $ case mbservername of
                                Just (s,_) -> Text.pack s
                                Nothing    -> ""
     commandSuccess st

cmdList :: NetworkCommand String
cmdList cs st rest =
  do sendMsg cs (ircList (Text.pack <$> words rest))
     commandSuccess st

cmdKill :: NetworkCommand (String, String)
cmdKill cs st (client,rest) =
  do sendMsg cs (ircKill (Text.pack client) (Text.pack rest))
     commandSuccess st

cmdAway :: NetworkCommand String
cmdAway cs st rest =
  do sendMsg cs (ircAway (Text.pack rest))
     commandSuccess st

cmdLinks :: NetworkCommand String
cmdLinks cs st rest =
  do sendMsg cs (ircLinks (Text.pack <$> words rest))
     commandSuccess st

cmdTime :: NetworkCommand (Maybe (String, ()))
cmdTime cs st arg =
  do sendMsg cs $ ircTime $
       case arg of
         Nothing    -> ""
         Just (x,_) -> Text.pack x
     commandSuccess st

cmdZnc :: NetworkCommand String
cmdZnc cs st rest =
  do sendMsg cs (ircZnc (Text.words (Text.pack rest)))
     commandSuccess st

cmdZncPlayback :: NetworkCommand (Maybe (String, Maybe (String, ())))
cmdZncPlayback cs st args =
  case args of

    -- request everything
    Nothing -> success "0"

    -- current date explicit time
    Just (timeStr, Nothing)
       | Just tod <- parse timeFormats timeStr ->
          do now <- getZonedTime
             let (nowTod,t) = (zonedTimeLocalTime . localTimeTimeOfDay <<.~ tod) now
                 yesterday = over (zonedTimeLocalTime . localTimeDay) (addDays (-1))
                 fixDay
                   | tod <= nowTod = id
                   | otherwise     = yesterday
             successZoned (fixDay t)

    -- explicit date and time
    Just (dateStr, Just (timeStr, _))
       | Just day  <- parse dateFormats dateStr
       , Just tod  <- parse timeFormats timeStr ->
          do tz <- getCurrentTimeZone
             successZoned ZonedTime
               { zonedTimeZone = tz
               , zonedTimeToLocalTime = LocalTime
                   { localTimeOfDay = tod
                   , localDay       = day } }

    _ -> commandFailureMsg "unable to parse date/time arguments" st

  where
    -- %k doesn't require a leading 0 for times before 10AM
    timeFormats = ["%k:%M:%S","%k:%M"]
    dateFormats = ["%F"]
    parse formats str =
      asum (map (parseTimeM False defaultTimeLocale ?? str) formats)

    successZoned = success . formatTime defaultTimeLocale "%s"

    success start =
      do sendMsg cs (ircZnc ["*playback", "play", "*", Text.pack start])
         commandSuccess st

cmdMode :: NetworkCommand String
cmdMode cs st rest = modeCommand (Text.pack <$> words rest) cs st

cmdNick :: NetworkCommand (String, ())
cmdNick cs st (nick,_) =
  do sendMsg cs (ircNick (Text.pack nick))
     commandSuccess st

cmdPart :: ChannelCommand String
cmdPart channelId cs st rest =
  do let msg = rest
     sendMsg cs (ircPart channelId (Text.pack msg))
     commandSuccess st

-- | This command is equivalent to chatting without a command. The primary use
-- at the moment is to be able to send a leading @/@ to chat easily.
cmdSay :: ChannelCommand String
cmdSay _ _ st rest = executeChat rest st

cmdInvite :: ChannelCommand (String, ())
cmdInvite channelId cs st (nick,_) =
  do let freeTarget = has (csChannels . ix channelId . chanModes . ix 'g') cs
         cmd = ircInvite (Text.pack nick) channelId
     cs' <- if freeTarget
              then cs <$ sendMsg cs cmd
              else sendModeration channelId [cmd] cs
     commandSuccessUpdateCS cs' st

commandSuccessUpdateCS :: NetworkState -> ClientState -> IO CommandResult
commandSuccessUpdateCS cs st =
  do let networkId = view csNetworkId cs
     commandSuccess
       $ setStrict (clientConnections . ix networkId) cs st

cmdTopic :: ChannelCommand String
cmdTopic channelId cs st rest =
  do sendTopic channelId (Text.pack rest) cs
     commandSuccess st

tabTopic ::
  Bool {- ^ reversed -} ->
  ChannelCommand String
tabTopic _ channelId cs st rest

  | all (==' ') rest
  , Just topic <- preview (csChannels . ix channelId . chanTopic) cs =
     do let textBox = set Edit.line (Edit.endLine $ "/topic " ++ Text.unpack topic)
        commandSuccess (over clientTextBox textBox st)

  | otherwise = commandFailure st


cmdUsers :: ChannelCommand ()
cmdUsers _ _ st _ = commandSuccess (changeSubfocus FocusUsers st)

cmdChannelInfo :: ChannelCommand ()
cmdChannelInfo _ _ st _ = commandSuccess (changeSubfocus FocusInfo st)

cmdMasks :: ChannelCommand (String,())
cmdMasks channel cs st (rest,_) =
  case rest of
    [mode] | mode `elem` view (csModeTypes . modesLists) cs ->

        do let connecting = has (csPingStatus . _PingConnecting) cs
               listLoaded = has (csChannels . ix channel . chanLists . ix mode) cs
           unless (connecting || listLoaded)
             (sendMsg cs (ircMode channel [Text.singleton mode]))

           commandSuccess (changeSubfocus (FocusMasks mode) st)

    _ -> commandFailureMsg "unknown mask mode" st

cmdKick :: ChannelCommand (String, String)
cmdKick channelId cs st (who,reason) =
  do let msg = Text.pack reason
         cmd = ircKick channelId (Text.pack who) msg
     cs' <- sendModeration channelId [cmd] cs
     commandSuccessUpdateCS cs' st


cmdKickBan :: ChannelCommand (String, String)
cmdKickBan channelId cs st (who,reason) =
  do let msg = Text.pack reason

         whoTxt     = Text.pack who

         mask = renderUserInfo (computeBanUserInfo (mkId whoTxt) cs)
         cmds = [ ircMode channelId ["b", mask]
                , ircKick channelId whoTxt msg
                ]
     cs' <- sendModeration channelId cmds cs
     commandSuccessUpdateCS cs' st

computeBanUserInfo :: Identifier -> NetworkState    -> UserInfo
computeBanUserInfo who cs =
  case view (csUser who) cs of
    Nothing                   -> UserInfo who "*" "*"
    Just (UserAndHost _ host) -> UserInfo "*" "*" host

cmdRemove :: ChannelCommand (String, String)
cmdRemove channelId cs st (who,reason) =
  do let msg = Text.pack reason
         cmd = ircRemove channelId (Text.pack who) msg
     cs' <- sendModeration channelId [cmd] cs
     commandSuccessUpdateCS cs' st

cmdKnock :: NetworkCommand (String, String)
cmdKnock cs st (chan,message) =
  do sendMsg cs (ircKnock (Text.pack chan) (Text.pack message))
     commandSuccess st

cmdJoin :: NetworkCommand (String, Maybe (String, ()))
cmdJoin cs st (channels, mbKeys) =
  do let network = view csNetwork cs
     let channelId = mkId (Text.pack (takeWhile (/=',') channels))
     sendMsg cs (ircJoin (Text.pack channels) (Text.pack . fst <$> mbKeys))
     commandSuccess
        $ changeFocus (ChannelFocus network channelId) st


-- | @/query@ command. Takes a channel or nickname and switches
-- focus to that target on the current network.
cmdQuery :: ClientCommand (String, ())
cmdQuery st (channel, _) =
  case parseFocus (views clientFocus focusNetwork st) channel of
    Just focus -> commandSuccess (changeFocus focus st)
    Nothing    -> commandFailureMsg "No current network" st

-- | Tab completion for @/query@
tabQuery ::
  Bool {- ^ reversed order -} ->
  ClientCommand String
tabQuery isReversed st _ =
  simpleTabCompletion plainWordCompleteMode [] completions isReversed st
  where
    completions = currentNet <> allWindows
    allWindows  = renderSplitFocus <$> views clientWindows Map.keys st
    currentNet  = case views clientFocus focusNetwork st of
                    Just net -> idText <$> channelWindowsOnNetwork net st
                    Nothing  -> []

-- | Return the list of identifiers for open channel windows on
-- the given network name.
channelWindowsOnNetwork ::
  Text         {- ^ network              -} ->
  ClientState  {- ^ client state         -} ->
  [Identifier] {- ^ open channel windows -}
channelWindowsOnNetwork network st =
  [ chan | ChannelFocus net chan <- Map.keys (view clientWindows st)
         , net == network ]


cmdQuit :: NetworkCommand String
cmdQuit cs st rest =
  do let msg = Text.pack rest
     sendMsg cs (ircQuit msg)
     commandSuccess st

cmdDisconnect :: NetworkCommand ()
cmdDisconnect cs st _ =
  do st' <- abortNetwork (view csNetwork cs) st
     commandSuccess st'

-- | Reconnect to the currently focused network. It's possible
-- that we're not currently connected to a network, so
-- this is implemented as a client command.
cmdReconnect :: ClientCommand ()
cmdReconnect st _
  | Just network <- views clientFocus focusNetwork st =

      do tm <- getCurrentTime
         st' <- addConnection 0 (Just tm) network =<< abortNetwork network st
         commandSuccess
           $ changeFocus (NetworkFocus network) st'

  | otherwise = commandFailureMsg "command requires focused network" st

cmdIgnore :: ClientCommand String
cmdIgnore st rest =
  case mkId . Text.pack <$> words rest of
    [] -> commandFailureMsg "bad arguments" st
    xs -> commandSuccess
            $ over clientIgnores updateIgnores st
      where
        updateIgnores :: HashSet Identifier -> HashSet Identifier
        updateIgnores s = foldl' updateIgnore s xs
        updateIgnore s x = over (contains x) not s

-- | Implementation of @/reload@
--
-- Attempt to reload the configuration file
cmdReload :: ClientCommand (Maybe (String, ()))
cmdReload st mbPath =
  do let path = fst <$> mbPath
            <|> view (clientConfig . configConfigPath) st
     res <- loadConfiguration path
     case res of
       Left e    -> commandFailureMsg (describeProblem e) st
       Right cfg ->
         do st1 <- clientStartExtensions (set clientConfig cfg st)
            commandSuccess st1

  where
    describeProblem err =
      Text.pack $
      case err of
       ConfigurationReadFailed    e -> "Failed to open configuration: "  ++ e
       ConfigurationParseFailed _ e -> "Failed to parse configuration: " ++ e
       ConfigurationMalformed   _ e -> "Configuration malformed: "       ++ e

-- | Support file name tab completion when providing an alternative
-- configuration file.
--
-- /NOT IMPLEMENTED/
tabReload :: Bool {- ^ reversed -} -> ClientCommand String
tabReload _ st _ = commandFailure st

modeCommand ::
  [Text] {- mode parameters -} ->
  NetworkState                 ->
  ClientState                  ->
  IO CommandResult
modeCommand modes cs st =
  case view clientFocus st of

    NetworkFocus _ ->
      do sendMsg cs (ircMode (view csNick cs) modes)
         commandSuccess st

    ChannelFocus _ chan ->
      case modes of
        [] -> success False [[]]
        flags:params ->
          case splitModes (view csModeTypes cs) flags params of
            Nothing -> commandFailureMsg "failed to parse modes" st
            Just parsedModes ->
              success needOp (unsplitModes <$> chunksOf (view csModeCount cs) parsedModes')
              where
                parsedModes'
                  | useChanServ chan cs = filter (not . isOpMe) parsedModes
                  | otherwise           = parsedModes

                needOp = not (all isPublicChannelMode parsedModes)
      where
        isOpMe (True, 'o', param) = mkId param == view csNick cs
        isOpMe _                  = False

        success needOp argss =
          do let cmds = ircMode chan <$> argss
             cs' <- if needOp
                      then sendModeration chan cmds cs
                      else cs <$ traverse_ (sendMsg cs) cmds
             commandSuccessUpdateCS cs' st

    _ -> commandFailure st

tabMode :: Bool -> NetworkCommand String
tabMode isReversed cs st rest =
  case view clientFocus st of

    ChannelFocus _ channel
      | flags:params     <- Text.words (Text.pack rest)
      , Just parsedModes <- splitModes (view csModeTypes cs) flags params
      , let parsedModesWithParams =
              [ (pol,mode) | (pol,mode,arg) <- parsedModes, not (Text.null arg) ]
      , (pol,mode):_      <- drop (paramIndex-3) parsedModesWithParams
      , let (hint, completions) = computeModeCompletion pol mode channel cs st
      -> simpleTabCompletion plainWordCompleteMode hint completions isReversed st

    _ -> commandFailure st

  where
    paramIndex = length $ words $ uncurry take $ clientLine st

activeNicks ::
  ClientState ->
  [Identifier]
activeNicks st =
  case view clientFocus st of
    focus@(ChannelFocus network channel) ->
      toListOf
        ( clientWindows    . ix focus
        . winMessages      . each
        . wlSummary        . folding summaryActor
        . filtered isActive
        . filtered isNotSelf ) st
      where
        isActive n = HashMap.member n userMap
        self = preview ( clientConnection network . csNick ) st
        isNotSelf n = case self of
                        Nothing -> True
                        Just s -> n /= s
        userMap = view ( clientConnection network
                       . csChannels . ix channel
                       . chanUsers) st

    _ -> []

  where
    -- Returns the 'Identifier' of the nickname responsible for
    -- the window line when that action was significant enough to
    -- be considered a hint for tab completion.
    summaryActor :: IrcSummary -> Maybe Identifier
    summaryActor (ChatSummary who) = Just who
    summaryActor _                 = Nothing

-- | Use the *!*@host masks of users for channel lists when setting list modes
--
-- Use the channel's mask list for removing modes
--
-- Use the nick list otherwise
computeModeCompletion ::
  Bool {- ^ mode polarity -} ->
  Char {- ^ mode          -} ->
  Identifier {- ^ channel -} ->
  NetworkState    ->
  ClientState ->
  ([Identifier],[Identifier]) {- ^ (hint, complete) -}
computeModeCompletion pol mode channel cs st
  | mode `elem` view modesLists modeSettings =
        if pol then ([],usermasks) else ([],masks)
  | otherwise = (activeNicks st, nicks)
  where
    modeSettings = view csModeTypes cs
    nicks = HashMap.keys (view (csChannels . ix channel . chanUsers) cs)

    masks = mkId <$> HashMap.keys (view (csChannels . ix channel . chanLists . ix mode) cs)

    usermasks =
      [ mkId ("*!*@" <> host)
        | nick <- HashMap.keys (view (csChannels . ix channel . chanUsers) cs)
        , UserAndHost _ host <- toListOf (csUsers . ix nick) cs
        ]

-- | Predicate for mode commands that can be performed without ops
isPublicChannelMode :: (Bool, Char, Text) -> Bool
isPublicChannelMode (True, 'b', param) = Text.null param -- query ban list
isPublicChannelMode (True, 'q', param) = Text.null param -- query quiet list
isPublicChannelMode _                  = False

commandNameCompletion :: Bool -> ClientState -> Maybe ClientState
commandNameCompletion isReversed st =
  do guard (cursorPos == n)
     clientTextBox (wordComplete plainWordCompleteMode isReversed [] possibilities) st
  where
    n = length leadingPart
    (cursorPos, line) = clientLine st
    leadingPart = takeWhile (/=' ') line
    possibilities = Text.cons '/' <$> commandNames
    commandNames = keys commands
                ++ keys (view (clientConfig . configMacros) st)

-- | Complete the nickname at the current cursor position using the
-- userlist for the currently focused channel (if any)
nickTabCompletion :: Bool {- ^ reversed -} -> ClientState -> IO CommandResult
nickTabCompletion isReversed st =
  simpleTabCompletion mode hint completions isReversed st
  where
    hint          = activeNicks st
    completions   = currentCompletionList st
    mode          = currentNickCompletionMode st

cmdExtension :: ClientCommand (String, String)
cmdExtension st (name,params) =
  case find (\ae -> aeName ae == Text.pack name)
            (view (clientExtensions . esActive) st) of
        Nothing -> commandFailureMsg "unknown extension" st
        Just ae ->
          do (st',_) <- clientPark st $ \ptr ->
                          commandExtension ptr (Text.pack <$> words params) ae
             commandSuccess st'

-- | Implementation of @/exec@ command.
cmdExec :: ClientCommand String
cmdExec st rest =
  do now <- getZonedTime
     case parseExecCmd rest of
       Left es -> failure now es
       Right ec ->
         case buildTransmitter now ec of
           Left es -> failure now es
           Right tx ->
             do res <- runExecCmd ec
                case res of
                  Left es -> failure now es
                  Right msgs -> tx (map Text.pack msgs)

  where
    buildTransmitter now ec =
      case (Text.pack <$> view execOutputNetwork ec,
            Text.pack <$> view execOutputChannel ec) of
        (Nothing, Nothing) -> Right (sendToClient now)
        (Just network, Nothing) ->
          case preview (clientConnection network) st of
            Nothing -> Left ["Unknown network"]
            Just cs -> Right (sendToNetwork now cs)
        (Nothing , Just channel) ->
          case currentNetworkState of
            Nothing -> Left ["No current network"]
            Just cs -> Right (sendToChannel cs channel)
        (Just network, Just channel) ->
          case preview (clientConnection network) st of
            Nothing -> Left ["Unknown network"]
            Just cs -> Right (sendToChannel cs channel)

    sendToClient now msgs = commandSuccess $! foldl' (recordSuccess now) st msgs

    sendToNetwork now cs msgs =
      commandSuccess =<<
      foldM (\st1 msg ->
           case parseRawIrcMsg msg of
             Nothing ->
               return $! recordError now st1 ("Bad raw message: " <> msg)
             Just raw ->
               do sendMsg cs raw
                  return st1) st msgs

    sendToChannel cs channel msgs =
      commandSuccess =<<
      foldM (\st1 msg ->
        do sendMsg cs (ircPrivmsg channel msg)
           chatCommand'
              (\src tgt -> Privmsg src tgt msg)
              channel
              cs st1) st (filter (not . Text.null) msgs)

    currentNetworkState =
      do network <- views clientFocus focusNetwork st
         preview (clientConnection network) st

    failure now es =
      commandFailure $! foldl' (recordError now) st (map Text.pack es)

recordError :: ZonedTime -> ClientState -> Text -> ClientState
recordError now ste e =
  recordNetworkMessage ClientMessage
    { _msgTime    = now
    , _msgBody    = ErrorBody e
    , _msgNetwork = ""
    } ste

recordSuccess :: ZonedTime -> ClientState -> Text -> ClientState
recordSuccess now ste m =
  recordNetworkMessage ClientMessage
    { _msgTime    = now
    , _msgBody    = NormalBody m
    , _msgNetwork = ""
    } ste


cmdUrl :: ClientCommand (Maybe (String, ()))
cmdUrl st mbArg =
  case view (clientConfig . configUrlOpener) st of
    Nothing -> commandFailureMsg "url-opener not configured" st
    Just opener ->
      case mbArg of
        Nothing -> doUrlOpen opener 0
        Just (arg,_) ->
          case readMaybe arg of
            Just n | n > 0 -> doUrlOpen opener (n-1)
            _ -> commandFailureMsg "bad url number" st
  where
    focus = view clientFocus st

    urls = toListOf ( clientWindows . ix focus . winMessages . each . wlText
                    . folding urlMatches) st

    doUrlOpen opener n =
      case preview (ix n) urls of
        Just url -> openUrl opener (Text.unpack url) st
        Nothing  -> commandFailureMsg "bad url number" st

openUrl :: FilePath -> String -> ClientState -> IO CommandResult
openUrl opener url st =
  do res <- try (callProcess opener [url])
     case res of
       Left e  -> commandFailureMsg (Text.pack (displayException (e :: IOError))) st
       Right{} -> commandSuccess st

-- | Implementation of @/grep@ and @/grepi@
cmdGrep ::
  Bool {- ^ case sensitive -} ->
  ClientCommand String
cmdGrep sensitive st str
  | null str  = commandSuccess (set clientRegex Nothing st)
  | otherwise =
      case compile defaultCompOpt{caseSensitive=sensitive} defaultExecOpt str of
        Left e -> commandFailureMsg (Text.pack e) st
        Right r -> commandSuccess (set clientRegex (Just r) st)

cmdOper :: NetworkCommand (String, (String, ()))
cmdOper cs st (user, (pass, ())) =
  do sendMsg cs (ircOper (Text.pack user) (Text.pack pass))
     commandSuccess st
