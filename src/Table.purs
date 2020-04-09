module Table where

import Prelude
import Data.Array (filter, snoc, sort)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as B

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
  = Entries

data Action
  = HandleInput Input
  | SetHover HoverInfo

component :: forall q m. MonadEffect m => H.Component HH.HTML q Input Message m
component =
  H.mkComponent
    { initialState: identity
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
          $ map mkRow state
      ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  HandleInput entries ->
    -- No re-render
    H.put entries
    --H.put $ entries <> []
    --H.put $ identity entries

    -- Triggers re-render
    --H.put $ sort entries
    --H.put $ filter (const true) entries
    --H.put $ map identity entries
  SetHover h -> do
    H.raise (MessageHover h)
