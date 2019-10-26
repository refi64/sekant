-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Sekant.Run (runCommand, runEscalated, runSelfEscalated) where

import Data.Bool
import Control.Monad.Reader
import Sekant.Output
import Sekant.Shared
import System.Environment
import System.Exit
import System.Posix.User
import System.Process hiding (runCommand)

getIsRoot :: (MonadIO m) => m Bool
getIsRoot = liftIO $ (== 0) <$> (toInteger <$> getRealUserID)

runCommand :: (MonadReader Shared m, MonadIO m) => [String] -> m ()
runCommand command = do
  logDebug $ "Run " ++ (show command)
  exit <- liftIO $ spawnProcess (head command) (tail command) >>= waitForProcess
  case exit of
    ExitSuccess -> return ()
    ExitFailure ret -> do
      let exit' = ExitFailure $ bool id (256 +) (ret < 0) ret
      logDebug $ "Failed with exit status " ++ (show exit')
      liftIO $ exitWith exit'

runEscalated :: (MonadReader Shared m, MonadIO m) => [String] -> m ()
runEscalated command = do
  isRoot <- getIsRoot
  let command' = bool id ("sudo":) (not isRoot) command
  runCommand command'

runSelfEscalated :: (MonadReader Shared m, MonadIO m) => [String] -> m () -> m ()
runSelfEscalated options func = do
  isRoot <- getIsRoot
  if isRoot
  then func
  else do
    exe <- liftIO getExecutablePath
    shared <- ask
    let verbose = getSharedVerbose shared
    let color = getSharedColorsEnabled shared
    let command = exe:("--color=" ++ (if color then "yes" else "no"))
                     :(bool id ("-v":) verbose options)
    runEscalated command
