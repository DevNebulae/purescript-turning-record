module Main where

import Prelude
import Application.State (rotate)
import Application.Types (ApplicationState(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, info)
import Control.Monad.Eff.Now (NOW, now)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (Window)
import DOM.HTML.Window (RequestAnimationFrameId, document, requestAnimationFrame)
import DOM.Node.Element (setAttribute)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(ElementId), documentToNonElementParentNode)
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Unsafe.Coerce (unsafeCoerce)

initialState :: ApplicationState
initialState = ApplicationState 0.0 88.0

rotateRecord :: ∀ eff.
  	ApplicationState ->
	Maybe Element ->
	Eff
		( dom :: DOM
        | eff
        )
        Unit
rotateRecord (ApplicationState rotation _) element = do
	case element of
		Just el ->
			setAttribute "style" ("width: 100px; height: 100px; background-color: black; transform-origin: 50% 50%; transform: rotate(" <> show rotation <> "deg);") el

		Nothing -> pure unit

keepLogging :: ∀ eff. ApplicationState -> Milliseconds -> Milliseconds -> Window -> Eff ( dom :: DOM, console :: CONSOLE, now :: NOW | eff) Unit
keepLogging appState previous current window =
	void $ requestAnimationFrame
		do
			-- Get the current timestamp in milliseconds
			current' <- unInstant <$> now
			-- Get the document type of the HTML document
			doc <- unsafeCoerce $ document window
			-- Retrieve the record element
			element <- getElementById (ElementId "record") $ documentToNonElementParentNode doc
			-- Declare the current parameter timestamp as the previous timestamp
			let previous' = current
			-- Update the application state by rotating the record element
			let appState' = rotate previous' current' appState

			rotateRecord appState' element
			keepLogging appState' previous current' window
	window

main :: ∀ eff. Eff
	(dom :: DOM
	, console :: CONSOLE
	, now :: NOW
	| eff
	) RequestAnimationFrameId
main = do
	window <- window
	current <- unInstant <$> now
	requestAnimationFrame (keepLogging initialState current current window) window
