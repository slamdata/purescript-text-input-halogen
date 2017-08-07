module TextInput.Halogen.Component
  ( input
  , Query(GetValue, SetValue)
  , Message(..)
  , Config
  , Input
  , InputValue
  , HasTextInputVal
  )
  where

import Prelude

import Control.MonadPlus (guard)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


type State val = InputValue val
type InputValue val = Tuple (Maybe val) String

type HasTextInputVal a  =
  { fromString ∷ String → Maybe a
  , toString ∷ a → String
  }

data Message val = NotifyChange (Input val)
type Input val = Maybe val
data Query val next
  = GetValue (Input val → next)
  | SetValue (Input val) next
  | Update String next

type DSL val = H.ComponentDSL (State val) (Query val) (Message val)
type HTML val = H.ComponentHTML (Query val)

type Config =
  { title ∷ String
  , placeholder ∷ String
  , root ∷ Array ClassName
  , rootInvalid ∷ Array ClassName
  }

input ∷ ∀ val m
  . Eq val
  ⇒ HasTextInputVal val
  → Config
  → H.Component HH.HTML (Query val) Unit (Message val) m
input hasInputVal conf = H.component
  { initialState: const $ Tuple Nothing ""
  , render: render hasInputVal conf
  , eval: eval hasInputVal
  , receiver: const Nothing
  }

render ∷ ∀ val. HasTextInputVal val → Config → State val → HTML val
render hasInputVal conf value = HH.input
  [ HP.type_ HP.InputText
  , HP.classes $ conf.root <> (guard (isInvalid value) *> conf.rootInvalid)
  , HP.title conf.title
  , HP.placeholder conf.placeholder
  , HP.value $ snd value
  , HE.onValueInput $ HE.input Update
  ]

eval ∷ ∀ val m . Eq val ⇒ HasTextInputVal val → Query val ~> DSL val m
eval hasInputVal = case _ of
  SetValue val next → do
    prevVal ← H.get
    unless (fst prevVal == val) (H.put $ Tuple val (maybe "" hasInputVal.toString val))
    pure next
  GetValue next → H.get <#> (fst >>> next)
  Update str next → do
    prevVal ← H.get
    let nextVal = Tuple (hasInputVal.fromString str) str
    H.put nextVal
    unless (nextVal == prevVal) $ H.raise (NotifyChange $ fst nextVal)
    pure next

isInvalid  ∷ ∀ a. InputValue a → Boolean
isInvalid (Tuple _       "") = false
isInvalid (Tuple Nothing _) = true
isInvalid _ = false
