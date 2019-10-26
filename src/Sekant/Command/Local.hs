-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Sekant.Command.Local (runLocalCommand, LocalActionOptions(..), LocalAction(..)) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Reader
import Development.Shake
import Development.Shake.FilePath
import Sekant.Output
import Sekant.Run
import Sekant.Shared
import qualified System.Directory as D

data LocalActionOptions = LocalActionOptions { localChangeDir :: String
                                              ,localStateDir  :: String
                                              ,localPackages  :: [String] }

data LocalAction = LocalBuildAction
                 | LocalInstallAction { localInstallNoBuild :: Bool }
                 | LocalUninstallAction
                 | LocalCleanAction

getTypeEnforcementFiles :: (MonadReader Shared m, MonadIO m) => [String] -> m [String]
getTypeEnforcementFiles [] = do
  te <- liftIO $ getDirectoryFilesIO "." ["*.te"]
  when (length te == 0) $ die "No te files to build."
  return te
getTypeEnforcementFiles packages = mapM getTE packages
  where
    getTE pkg = do
      when ('/' `elem` pkg || '.' `elem` pkg) $ die $ "Invalid package name: " ++ pkg

      let te = pkg ++ ".te"
      whenM (liftIO $ not <$> D.doesFileExist te) . die $ "Package does not exist: " ++ pkg

      return te

runShake :: (MonadReader Shared m, MonadIO m) => [String] -> ShakeOptions -> m ()
runShake targets options = do
  shared <- ask
  liftIO . shake options $ do
    action $ need targets

    "*.pp" %> \out -> do
      let mod = out -<.> ".mod"
      need [mod]
      flip runReaderT shared $ logRun "semodule_package" out
      cmd_ "semodule_package -m" [mod] "-o" [out]

    "*.mod" %> \out -> do
      let te = out -<.> ".te"
      need [te]
      flip runReaderT shared  $ logRun "checkmodule" out
      cmd_ "checkmodule -m" [te] "-o" [out]

    phony "clean" $ removeFilesAfter "." ["//*.pp", "//*.mod"]

runSemodule :: (MonadReader Shared m, MonadIO m) => String -> String -> [String] -> m ()
runSemodule what flag args = do
  logRun "semodule" $ what ++ " " ++ unwords args
  runEscalated $ "semodule":(concat $ map ((flag:) . pure) args)

runLocalCommand :: (MonadReader Shared m, MonadIO m) => LocalAction -> LocalActionOptions -> m ()
runLocalCommand act (LocalActionOptions changeDir stateDir packages) = do
  liftIO $ D.setCurrentDirectory changeDir
  let customShakeOptions = shakeOptions { shakeFiles     = stateDir
                                        , shakeVerbosity = Quiet
                                        , shakeProgress  = progressSimple }

  te <- getTypeEnforcementFiles packages
  let pp = map (-<.> ".pp") te
  let names = map (-<.> "") te

  case act of
    LocalBuildAction ->
      runShake pp customShakeOptions
    LocalInstallAction nobuild -> do
      when (not nobuild) $ runShake pp customShakeOptions
      runSemodule "install" "-i" pp
    LocalUninstallAction ->
      runSemodule "remove" "-r" names
    LocalCleanAction ->
      runShake ["clean"] customShakeOptions
