module Example where

import Prelude
import Data.Array ((..))
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

type State
  = { hover :: HoverInfo
    , entries :: Array Int
    }

data Action
  = SetHover HoverInfo

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const { hover: NoHover, entries: 1 .. 1000 }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall m. State -> H.ComponentHTML Action () m
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
      [ HP.classes [ B.table, B.tableSm, B.tableHover ]
      , HE.onMouseLeave \_ -> Just (SetHover NoHover)
      ]
      [ HH.thead_
          [ HH.tr_
              $ map
                  (\str -> HH.th [ HP.classes [ B.colSm1 ] ] [ HH.text str ])
                  [ "id", "value" ]
          ]
      , HH.tbody_ $ map mkRow state.entries
      ]

handleAction ∷ forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  SetHover h -> H.modify_ \st -> st { hover = h }
