-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Monad.Reader
import Options.Applicative
import Sekant.Command.Audit
import Sekant.Command.Local
import Sekant.Shared
import System.Posix.IO
import System.Posix.Terminal

data Command = LocalCommand LocalAction LocalActionOptions
             | AuditCommand AuditAction

data TriChoice = TriYes
               | TriNo
               | TriAuto

triChoice :: ReadM TriChoice
triChoice = str >>= \case
  "yes"  -> return TriYes
  "no"   -> return TriNo
  "auto" -> return TriAuto
  _      -> readerError "Option must be 'yes', 'no', or 'auto'"

fillTriAuto :: Bool -> TriChoice -> Bool
fillTriAuto _ TriYes  = True
fillTriAuto _ TriNo   = False
fillTriAuto d TriAuto = d

fillTriAutoM :: (Monad m) => m Bool -> TriChoice -> m Bool
fillTriAutoM d t = flip fillTriAuto t <$> d

data Options = Options { optCommand :: Command
                        ,optVerbose :: Bool
                        ,optColors  :: TriChoice }

localCommand = LocalCommand <$> (subcommands <**> helper) <*> opts
  where
    opts = LocalActionOptions
      <$> (strOption $
            long "directory"
            <> short 'C'
            <> metavar "DIRECTORY"
            <> value "."
            <> help "Change to the given directory first")
      <*> (strOption $
            long "state"
            <> metavar "DIRECTORY"
            <> value ".sekant"
            <> help "Use the given directory as the state dir")
      <*> (many . argument str $
            metavar "PACKAGES...")

    subcommands = subparser $
      (   command "build" (info (pure LocalBuildAction)
                                $ progDesc "Build the local policy modules")
       <> command "install" (info (LocalInstallAction
                                    <$> (switch $
                                          long "nobuild"
                                          <> help "Don't try to rebuild first"))
                                  $ progDesc "Install the local modules")
      <> command "uninstall" (info (pure LocalUninstallAction)
                                    $ progDesc "Uninstall the local policy modules")
      <> command "clean" (info (pure LocalCleanAction)
                                $ progDesc "Clean the build policy modules"))

auditCommand = AuditCommand <$> subcommands <**> helper
  where
    subcommands = subparser $
      (   command "clear" (info (pure AuditClearAction)
                                $ progDesc "Clear the audit log")
       <> command "explain" (info (pure AuditExplainAction)
                                  $ progDesc "Explain the audit log"))

globalOpts = info (options <**> helper) $ fullDesc <> header "sekant - help managing SELinux"
  where
    options = Options
                <$> commands
                <*> (switch $
                      long "verbose"
                      <> short 'v'
                      <> help "Enable verbose mode")
                <*> (option triChoice $
                      long "color"
                      <> metavar "COLOR"
                      <> value TriAuto
                      <> help "Control the use of ANSI colors")
    commands = subparser $
      (   command "local" (info localCommand $ progDesc "Build and install custom policies")
       <> command "audit" (info auditCommand $ progDesc "Work with the audit log"))

runCommand (LocalCommand act options) = runLocalCommand act options
runCommand (AuditCommand act) = runAuditCommand act

main :: IO ()
main = do
  (Options command verbose colorsChoice) <- execParser globalOpts
  colors <- fillTriAutoM (queryTerminal stdOutput) colorsChoice

  let shared = Shared verbose colors
  flip runReaderT shared $ case command of
    LocalCommand options act -> runLocalCommand options act
    AuditCommand         act -> runAuditCommand act
