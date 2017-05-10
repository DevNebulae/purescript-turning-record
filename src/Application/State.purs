module Application.State
	( initApplicationState
	, rotate
	) where

import Application.Types (ApplicationState(..))
import Data.Time.Duration (Milliseconds(..))
import Math ((%))
import Prelude ((*), (+), (-), (/))

initApplicationState :: Number -> Number -> ApplicationState
initApplicationState rpm rotation = ApplicationState rpm rotation

rotate :: Milliseconds -> Milliseconds -> ApplicationState -> ApplicationState
rotate (Milliseconds previous) (Milliseconds current) (ApplicationState rotation rpm) = initApplicationState rotation' rpm
	where
		increase = rpm / 60000.0 * (current - previous) * 360.0
		rotation' = (rotation + increase) % 360.0

