module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (RequestAnimationFrameId, requestAnimationFrame)

main :: forall e. Eff (dom :: DOM, console :: CONSOLE | e) RequestAnimationFrameId
main = do
	window <- window
	requestAnimationFrame (log "Hello world!") window
