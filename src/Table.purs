module Table where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as B
import Unsafe.Reference (unsafeRefEq)

data HoverInfo
  = HoverRow Int
  | NoHover

data Message
  = MessageHover HoverInfo

type Slot
  = H.Slot (Const Void) Message

type Entries
  = Array Int

type Input
  = Entries

type State
  = { entries :: Entries }

data Action
  = HandleInput Input
  | SetHover HoverInfo

component :: forall q m. MonadEffect m => H.Component HH.HTML q Input Message m
component =
  H.mkComponent
    { initialState: { entries: _ }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< HandleInput
              }
    }

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action () m
render state =
  let
    mkRow idx =
      HH.tr
        [ HE.onMouseEnter \_ -> Just (SetHover (HoverRow idx)) ]
        $ map
            (\str -> HH.td_ [ HH.text str ])
            [ show idx, show $ idx * 10 ]
  in
    HH.table
      [ HP.classes [ B.table, B.tableSm, B.tableHover ] ]
      [ HH.thead_
          [ HH.tr_
              $ map
                  (\str -> HH.th [ HP.classes [ B.colSm1 ] ] [ HH.text str ])
                  [ "id", "value" ]
          ]
      , HH.tbody
          [ HE.onMouseLeave \_ -> Just (SetHover NoHover) ]
          $ map mkRow state.entries
      ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  HandleInput entries -> do
    old <- H.get
    unless (unsafeRefEq old.entries entries) $ H.put { entries }
  SetHover h -> do
    H.raise (MessageHover h)
