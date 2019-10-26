-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Sekant.Output (hPutTermDoc, hPutTermDocLn, logDebug, logRun, die) where

import Control.Monad.Extra
import Control.Monad.Reader
import Data.Bool
import Data.Maybe
import Sekant.Shared
import System.Exit (exitFailure)
import System.IO
import System.Console.Terminal.Size
import qualified Text.PrettyPrint.ANSI.Leijen as P

hPutTermDoc :: (MonadReader Shared m, MonadIO m) => Handle -> P.Doc -> m ()
hPutTermDoc handle doc = do
  window <- liftIO $ hSize handle
  let w = fromMaybe 80 $ width <$> window
  colors <- getSharedColorsEnabled <$> ask
  let doc' = bool id P.plain (not colors) doc
  liftIO . P.displayIO handle $ P.renderPretty 0.4 w doc'

hPutTermDocLn :: (MonadReader Shared m, MonadIO m) => Handle -> P.Doc -> m ()
hPutTermDocLn handle doc = do
  hPutTermDoc handle doc
  liftIO $ hPutStrLn handle ""

logDebug :: (MonadReader Shared m, MonadIO m) => String -> m ()
logDebug msg =
  whenM (getSharedVerbose <$> ask) $
    hPutTermDocLn stderr . P.blue $ P.text msg

logRun :: (MonadReader Shared m, MonadIO m) => String -> String -> m ()
logRun descr output =
  hPutTermDocLn stderr $
    (P.indent 2 . P.bold $ P.text descr)
    P.<+> (P.text "=>")
    P.<+> (P.green $ P.text output)

die :: (MonadReader Shared m, MonadIO m) => String -> m ()
die msg = do
  hPutTermDocLn stderr . P.red $ P.text msg
  liftIO exitFailure
