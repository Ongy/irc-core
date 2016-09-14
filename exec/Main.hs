{-|
Module      : Main
Description : Entry-point of executable
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Entry point into glirc2. This module sets up VTY and launches the client.
-}

module Main where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.List (nub)
import Data.Text (Text)
import System.Exit
import System.IO

import Client.Configuration
import Client.EventLoop
import Client.Options
import Client.State
import Client.State.Focus

-- | Main action for IRC client
main :: IO ()
main =
  do opts <- getOptions
     cfg  <- loadConfiguration' (view optConfigFile opts)
     runInUnboundThread $
       withClientState cfg $
       clientStartExtensions >=>
       addInitialNetworks (view optInitialNetworks opts) >=>
       eventLoop

-- | Load configuration and handle errors along the way.
loadConfiguration' :: Maybe FilePath -> IO Configuration
loadConfiguration' path =
  do cfgRes <- loadConfiguration path
     case cfgRes of
       Right cfg -> return cfg
       Left (ConfigurationReadFailed e) ->
         report "Failed to open configuration:" e
       Left (ConfigurationParseFailed e) ->
         report "Failed to parse configuration:" e
       Left (ConfigurationMalformed e) ->
         report "Configuration malformed: " e
  where
    report problem msg =
      do hPutStrLn stderr problem
         hPutStrLn stderr msg
         exitFailure

-- | Create connections for all the networks on the command line.
-- Set the client focus to the first network listed.
addInitialNetworks ::
  [Text] {- networks -} ->
  ClientState           ->
  IO ClientState
addInitialNetworks optNetworks st =
  case nub (clientAutoconnects st ++ optNetworks) of
    []        -> return st
    networks  ->
      do st' <- foldM (flip (addConnection 0 Nothing)) st networks
         return (set clientFocus (NetworkFocus (head networks)) st')
