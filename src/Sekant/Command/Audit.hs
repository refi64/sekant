-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Sekant.Command.Audit (runAuditCommand, AuditAction(..)) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List
import Development.Shake
import Development.Shake.FilePath
import Sekant.Output
import Sekant.Run
import Sekant.Shared
import System.Directory

data AuditAction = AuditClearAction
                  | AuditExplainAction

deleteAuditLogs :: (MonadIO m) => m ()
deleteAuditLogs =
  liftIO $ getDirectoryFilesIO logDir ["audit.log*"] >>= mapM_ (clearLogFile . (logDir </>))
  where
    logDir = "/var/log/audit"
    clearLogFile path
      | takeFileName path == "audit.log" = writeFile path ""
      | otherwise                        = removeFile path

runAuditCommand :: (MonadReader Shared m, MonadIO m) => AuditAction -> m ()
runAuditCommand AuditClearAction = runSelfEscalated ["audit", "clear"] deleteAuditLogs
runAuditCommand AuditExplainAction = runEscalated ["audit2why", "-a"]
