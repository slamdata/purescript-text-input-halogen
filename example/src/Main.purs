module Main where

import Prelude

import TextInput.Halogen.Component as Txt
import Control.Monad.Eff (Eff)
import Data.Either.Nested as Either
import Data.Functor.Coproduct.Nested as Coproduct
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main ∷ Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI example unit body

data Query a
  = HandleStringMsg Int (Txt.Message String) a
  | HandleUnitMsg Int (Txt.Message Unit) a

type State = {}
type ChildQuery = Coproduct.Coproduct2 (Txt.Query String) (Txt.Query Unit)
type Slot = Either.Either2 Int Int


cpString ∷ CP.ChildPath (Txt.Query String) ChildQuery Int Slot
cpString = CP.cp1

cpUnit ∷ CP.ChildPath (Txt.Query Unit) ChildQuery Int Slot
cpUnit = CP.cp2


type HTML m = H.ParentHTML Query ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Void m


example ∷ ∀ m. H.Component HH.HTML Query Unit Void m
example = H.parentComponent
    { initialState: const {}
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ ∀ m. State → HTML m
render _ = HH.div_
  [ HH.h1_ [ HH.text "input 1" ]
  , HH.slot' cpString 0 (Txt.input hasTextInputValId baseConfig) unit (HE.input (HandleStringMsg 0))
  , HH.h1_ [ HH.text "input 2" ]
  , HH.slot' cpUnit 1 (Txt.input hasTextInputValRed redConfig) unit (HE.input (HandleUnitMsg 1))
  ]

hasTextInputValId :: Txt.HasTextInputVal String
hasTextInputValId = {toString: id, fromString: Just}

hasTextInputValRed :: Txt.HasTextInputVal Unit
hasTextInputValRed =
  { toString: const "red"
  , fromString: \str -> if str == "red" then Just unit else Nothing }

eval ∷ ∀ m. Query ~> DSL m
eval (HandleStringMsg _ _ next) = pure next
eval (HandleUnitMsg _ _ next) = pure next

redConfig :: Txt.Config
redConfig = baseConfig { placeholder =  "red" }

baseConfig :: Txt.Config
baseConfig =
  { title: ""
  , placeholder: ""
  , root: [HH.ClassName "TextInput"]
  , rootInvalid: [HH.ClassName "TextInput--invalid"]
  }
