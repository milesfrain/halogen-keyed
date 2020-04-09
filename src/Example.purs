module Example where

import Prelude

import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as B
import Table as Table

type State
  = { hover :: Table.HoverInfo
    , entries :: Array Int
    }

data Action = HandleTable Table.Message

type ChildSlots =
  ( table :: Table.Slot Unit)

_table = SProxy :: SProxy "table"

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const { hover: Table.NoHover, entries: 1 .. 1000 }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
render state =
  let
    hovTxt = case state.hover of
      Table.HoverRow i -> show i
      Table.NoHover -> "Not hovering"
  in
    HH.div [ HP.class_ B.col ]
      [ HH.div_ [ HH.text hovTxt ]
      , HH.slot _table unit Table.component state.entries (Just <<< HandleTable)
      ]

handleAction ∷ forall o m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  HandleTable (Table.MessageHover h) -> do
    log case h of
      Table.HoverRow i -> show i
      Table.NoHover -> "exit"
    H.modify_ \st -> st { hover = h }
