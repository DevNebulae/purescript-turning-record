module Application.Types where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Prelude (class Eq, class Show)

data ApplicationState = ApplicationState Number Number

derive instance eqApplicationState :: Eq ApplicationState
derive instance genericApplicationState :: Generic ApplicationState _

instance showApplicationState :: Show ApplicationState where
	show = genericShow
