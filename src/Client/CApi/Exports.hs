{-# Language RecordWildCards #-}
{-|
Module      : Client.CApi.Exports
Description : Foreign exports which expose functionality for extensions
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module exports the C functions that extensions can used to query the state
of the client.
-}
module Client.CApi.Exports
 ( -- * Extension API types
   Glirc_send_message
 , glirc_send_message

 , Glirc_print
 , glirc_print

 , Glirc_list_networks
 , glirc_list_networks

 , Glirc_list_channels
 , glirc_list_channels

 , Glirc_list_channel_users
 , glirc_list_channel_users

 , Glirc_my_nick
 , glirc_my_nick

 , Glirc_identifier_cmp
 , glirc_identifier_cmp

 , Glirc_is_channel
 , glirc_is_channel

 , Glirc_is_logged_on
 , glirc_is_logged_on

 , Glirc_mark_seen
 , glirc_mark_seen

 , Glirc_clear_window
 , glirc_clear_window

 , Glirc_current_focus
 , glirc_current_focus

 , Glirc_free_string
 , glirc_free_string

 , Glirc_free_strings
 , glirc_free_strings

 , Glirc_inject_chat
 , glirc_inject_chat
 ) where

import           Client.CApi.Types
import           Client.Message
import           Client.State
import           Client.State.Channel
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Lens
import           Control.Monad (unless)
import           Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import           Data.Time
import           Foreign.C
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.Storable
import           Irc.Identifier
import           Irc.RawIrcMsg
import           Irc.UserInfo
import           Irc.Message
import           LensUtils

------------------------------------------------------------------------

-- | Dereference the stable pointer passed to extension callbacks
derefToken :: Ptr () -> IO (MVar ClientState)
derefToken = deRefStablePtr . castPtrToStablePtr


------------------------------------------------------------------------

-- | Import a 'FgnMsg' into an 'RawIrcMsg'
peekFgnMsg :: FgnMsg -> IO RawIrcMsg
peekFgnMsg FgnMsg{..} =
  do let strArray n p = traverse peekFgnStringLen =<<
                        peekArray (fromIntegral n) p

     tagKeys <- strArray fmTagN fmTagKeys
     tagVals <- strArray fmTagN fmTagVals
     prefixN  <- peekFgnStringLen fmPrefixNick
     prefixU  <- peekFgnStringLen fmPrefixUser
     prefixH  <- peekFgnStringLen fmPrefixHost
     command <- peekFgnStringLen fmCommand
     params  <- strArray fmParamN fmParams

     return RawIrcMsg
       { _msgTags    = zipWith TagEntry tagKeys tagVals
       , _msgPrefix  = if Text.null prefixN
                         then Nothing
                         else Just (UserInfo (mkId prefixN) prefixU prefixH)
       , _msgCommand = command
       , _msgParams  = params
       }

------------------------------------------------------------------------

-- | Peek a 'FgnStringLen' as UTF-8 encoded bytes.
peekFgnStringLen :: FgnStringLen -> IO Text
peekFgnStringLen (FgnStringLen ptr len) =
  Text.peekCStringLen (ptr, fromIntegral len)

------------------------------------------------------------------------

-- | Network, command, and parameters are used when transmitting a message.
type Glirc_send_message =
  Ptr ()     {- ^ api token          -} ->
  Ptr FgnMsg {- ^ pointer to message -} ->
  IO CInt    {- ^ 0 on success       -}

glirc_send_message :: Glirc_send_message
glirc_send_message token msgPtr =
  do mvar    <- derefToken token
     fgn     <- peek msgPtr
     msg     <- peekFgnMsg fgn
     network <- peekFgnStringLen (fmNetwork fgn)
     withMVar mvar $ \st ->
       case preview (clientConnection network) st of
         Nothing -> return 1
         Just cs -> do sendMsg cs msg
                       return 0
  `catch` \SomeException{} -> return 1

------------------------------------------------------------------------

-- | Print a message or error to the client window
type Glirc_print =
  Ptr ()  {- ^ api token         -} ->
  MessageCode {- ^ enum message_code -} ->
  CString {- ^ message           -} ->
  CSize   {- ^ message length    -} ->
  IO CInt {- ^ 0 on success      -}

glirc_print :: Glirc_print
glirc_print stab code msgPtr msgLen =
  do mvar <- derefToken stab
     txt  <- peekFgnStringLen (FgnStringLen msgPtr msgLen)
     now  <- getZonedTime

     let con | code == normalMessage = NormalBody
             | otherwise             = ErrorBody
         msg = ClientMessage
                 { _msgBody    = con txt
                 , _msgTime    = now
                 , _msgNetwork = Text.empty
                 }
     modifyMVar_ mvar $ \st ->
       do return (recordNetworkMessage msg st)
     return 0
  `catch` \SomeException{} -> return 1

------------------------------------------------------------------------

-- | Print a message or error to the client window
type Glirc_inject_chat =
  Ptr ()  {- ^ api token         -} ->
  CString {- ^ network           -} ->
  CSize   {- ^ network length    -} ->
  CString {- ^ source            -} ->
  CSize   {- ^ source length     -} ->
  CString {- ^ target            -} ->
  CSize   {- ^ target length     -} ->
  CString {- ^ message           -} ->
  CSize   {- ^ message length    -} ->
  IO CInt {- ^ 0 on success      -}

glirc_inject_chat :: Glirc_inject_chat
glirc_inject_chat stab netPtr netLen srcPtr srcLen tgtPtr tgtLen msgPtr msgLen =
  do mvar <- derefToken stab
     net  <- peekFgnStringLen (FgnStringLen netPtr netLen)
     src  <- peekFgnStringLen (FgnStringLen srcPtr srcLen)
     tgt  <- mkId <$> peekFgnStringLen (FgnStringLen tgtPtr tgtLen)
     txt  <- peekFgnStringLen (FgnStringLen msgPtr msgLen)
     now  <- getZonedTime

     let msg = ClientMessage
                 { _msgBody    = IrcBody (Privmsg (parseUserInfo src) tgt txt)
                 , _msgTime    = now
                 , _msgNetwork = net
                 }
     modifyMVar_ mvar $ \st ->
       do return (recordChannelMessage net tgt msg st)
     return 0
  `catch` \SomeException{} -> return 1

------------------------------------------------------------------------

-- | The resulting strings and array of strings are malloc'd and the
-- caller must free them. NULL returned on failure.
type Glirc_list_networks =
  Ptr ()           {- ^ api token                                        -} ->
  IO (Ptr CString) {- ^ null terminated array of null terminated strings -}

glirc_list_networks :: Glirc_list_networks
glirc_list_networks stab =
  do mvar <- derefToken stab
     st   <- readMVar mvar
     let networks = views clientNetworkMap HashMap.keys st
     strs <- traverse (newCString . Text.unpack) networks
     newArray0 nullPtr strs

------------------------------------------------------------------------

-- | Case insensitive comparison suitable for use on channels and nicknames.
-- Returns -1 if the first identifier is less than the second
-- Returns 0 if the first identifier is equal to the second
-- Returns 1 if the first identifier is greater than the second
type Glirc_identifier_cmp =
  CString {- ^ identifier 1     -} ->
  CSize   {- ^ identifier 1 len -} ->
  CString {- ^ identifier 2     -} ->
  CSize   {- ^ identifier 2 len -} ->
  IO CInt

glirc_identifier_cmp :: Glirc_identifier_cmp
glirc_identifier_cmp p1 n1 p2 n2 =
  do txt1 <- peekFgnStringLen (FgnStringLen p1 n1)
     txt2 <- peekFgnStringLen (FgnStringLen p2 n2)
     return $! case compare (mkId txt1) (mkId txt2) of
                 LT -> -1
                 EQ ->  0
                 GT ->  1

------------------------------------------------------------------------

-- | The resulting strings and array of strings are malloc'd and the
-- caller must free them. NULL returned on failure.
type Glirc_list_channels =
  Ptr ()  {- ^ api token   -} ->
  CString {- ^ network     -} ->
  CSize   {- ^ network len -} ->
  IO (Ptr CString) {- ^ null terminated array of null terminated strings -}

glirc_list_channels :: Glirc_list_channels
glirc_list_channels stab networkPtr networkLen =
  do mvar <- derefToken stab
     st   <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen networkPtr networkLen)
     case preview (clientConnection network . csChannels) st of
        Nothing -> return nullPtr
        Just m  ->
          do strs <- traverse (newCString . Text.unpack . idText) (HashMap.keys m)
             newArray0 nullPtr strs

------------------------------------------------------------------------

-- | The resulting strings and array of strings are malloc'd and the
-- caller must free them.  NULL returned on failure.
type Glirc_list_channel_users =
  Ptr ()  {- ^ api token   -} ->
  CString {- ^ network     -} ->
  CSize   {- ^ network len -} ->
  CString {- ^ channel     -} ->
  CSize   {- ^ channel len -} ->
  IO (Ptr CString) {- ^ null terminated array of null terminated strings -}

glirc_list_channel_users :: Glirc_list_channel_users
glirc_list_channel_users stab networkPtr networkLen channelPtr channelLen =
  do mvar <- derefToken stab
     st   <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen networkPtr networkLen)
     channel <- peekFgnStringLen (FgnStringLen channelPtr channelLen)
     let mb = preview ( clientConnection network
                      . csChannels . ix (mkId channel)
                      . chanUsers
                      ) st
     case mb of
       Nothing -> return nullPtr
       Just m  ->
         do strs <- traverse (newCString . Text.unpack . idText) (HashMap.keys m)
            newArray0 nullPtr strs

------------------------------------------------------------------------

-- | The resulting string is malloc'd and the caller must free it.
-- NULL returned on failure.
type Glirc_my_nick =
  Ptr ()  {- ^ api token           -} ->
  CString {- ^ network name        -} ->
  CSize   {- ^ network name length -} ->
  IO CString

glirc_my_nick :: Glirc_my_nick
glirc_my_nick stab networkPtr networkLen =
  do mvar <- derefToken stab
     st   <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen networkPtr networkLen)
     let mb = preview (clientConnection network . csNick) st
     case mb of
       Nothing -> return nullPtr
       Just me -> newCString (Text.unpack (idText me))

------------------------------------------------------------------------

-- | Mark a window as being seen clearing the new message counter.
-- To clear the client window send an empty network name.
-- To clear a network window send an empty channel name.
type Glirc_mark_seen =
  Ptr ()  {- ^ api token           -} ->
  CString {- ^ network name        -} ->
  CSize   {- ^ network name length -} ->
  CString {- ^ channel name        -} ->
  CSize   {- ^ channel name length -} ->
  IO ()

glirc_mark_seen :: Glirc_mark_seen
glirc_mark_seen stab networkPtr networkLen channelPtr channelLen =
  do network <- peekFgnStringLen (FgnStringLen networkPtr networkLen)
     channel <- peekFgnStringLen (FgnStringLen channelPtr channelLen)

     let focus
           | Text.null network = Unfocused
           | Text.null channel = NetworkFocus network
           | otherwise         = ChannelFocus network (mkId channel)

     mvar <- derefToken stab
     modifyMVar_ mvar $ \st ->
       return $! overStrict (clientWindows . ix focus) windowSeen st

------------------------------------------------------------------------

-- | Mark a window as being seen clearing the new message counter.
-- To clear the client window send an empty network name.
-- To clear a network window send an empty channel name.
type Glirc_clear_window =
  Ptr ()  {- ^ api token           -} ->
  CString {- ^ network name        -} ->
  CSize   {- ^ network name length -} ->
  CString {- ^ channel name        -} ->
  CSize   {- ^ channel name length -} ->
  IO ()

glirc_clear_window :: Glirc_clear_window
glirc_clear_window stab networkPtr networkLen channelPtr channelLen =
  do network <- peekFgnStringLen (FgnStringLen networkPtr networkLen)
     channel <- peekFgnStringLen (FgnStringLen channelPtr channelLen)

     let focus
           | Text.null network = Unfocused
           | Text.null channel = NetworkFocus network
           | otherwise         = ChannelFocus network (mkId channel)

     mvar <- derefToken stab
     modifyMVar_ mvar $ \st ->
       return $! set (clientWindows . ix focus) emptyWindow st

------------------------------------------------------------------------

-- | Free one of the heap allocated strings found as a return value
-- from the extension API. If argument is NULL, nothing happens.
type Glirc_free_string =
  CString {- ^ glirc allocated string -} ->
  IO ()

glirc_free_string :: Glirc_free_string
glirc_free_string = free

------------------------------------------------------------------------

-- | Free an array of heap allocated strings found as a return value
-- from the extension API. If argument is NULL, nothing happens.
type Glirc_free_strings =
  Ptr CString {- ^ glirc allocated strings, null-terminated -} ->
  IO ()

glirc_free_strings :: Glirc_free_strings
glirc_free_strings p =
  unless (p == nullPtr) $
    do traverse_ free =<< peekArray0 nullPtr p
       free p

------------------------------------------------------------------------

-- | Free an array of heap allocated strings found as a return value
-- from the extension API. If argument is NULL, nothing happens.
--
-- Free the allocated strings with @glirc_free_string@
--
-- Strings set to NULL if there is no current network or no
-- current target.
type Glirc_current_focus =
  Ptr ()      {- ^ api token                      -} ->
  Ptr CString {- ^ newly allocated network string -} ->
  Ptr CSize   {- ^ network length                 -} ->
  Ptr CString {- ^ newly allocated target string  -} ->
  Ptr CSize   {- ^ target length                  -} ->
  IO ()

glirc_current_focus :: Glirc_current_focus
glirc_current_focus stab netP netL tgtP tgtL =
  do mvar <- derefToken stab
     st   <- readMVar mvar
     case view clientFocus st of
       Unfocused        -> do poke' netP nullPtr
                              poke' netL 0
                              poke' tgtP nullPtr
                              poke' tgtL 0

       NetworkFocus n   -> do exportText netP netL n
                              poke' tgtP nullPtr
                              poke' tgtL 0

       ChannelFocus n t -> do exportText netP netL n
                              exportText tgtP tgtL (idText t)

------------------------------------------------------------------------

-- | Returns @1@ when the given target on the given network is a channel
-- name, otherwise returns @0@.
--
-- If the given network is not currently active this returns @0@.
type Glirc_is_channel =
  Ptr ()  {- ^ api token       -} ->
  CString {- ^ network name    -} ->
  CSize   {- ^ network length  -} ->
  CString {- ^ target name     -} ->
  CSize   {- ^ target length   -} ->
  IO CInt {- ^ boolean         -}

glirc_is_channel :: Glirc_is_channel
glirc_is_channel stab net netL tgt tgtL =
  do mvar    <- derefToken stab
     st      <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen net netL)
     target  <- peekFgnStringLen (FgnStringLen tgt tgtL)

     case preview (clientConnection network) st of
       Just cs | isChannelIdentifier cs (mkId target) -> return 1
       _ -> return 0

------------------------------------------------------------------------

-- | Returns @1@ when the given target on the given network shares a
-- channel with the user, @0@ otherwise.
--
-- If the given network is not currently active this returns @0@.
type Glirc_is_logged_on =
  Ptr ()  {- ^ api token       -} ->
  CString {- ^ network name    -} ->
  CSize   {- ^ network length  -} ->
  CString {- ^ target name     -} ->
  CSize   {- ^ target length   -} ->
  IO CInt {- ^ boolean         -}

glirc_is_logged_on :: Glirc_is_channel
glirc_is_logged_on stab net netL tgt tgtL =
  do mvar    <- derefToken stab
     st      <- readMVar mvar
     network <- peekFgnStringLen (FgnStringLen net netL)
     target  <- peekFgnStringLen (FgnStringLen tgt tgtL)

     let online = has (clientConnection network . csUsers . ix (mkId target)) st
     return $! if online then 1 else 0
