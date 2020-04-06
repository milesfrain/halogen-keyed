module Example where

import Prelude
import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Themes.Bootstrap4 as B
import Halogen.HTML.Properties as HP

data HoverInfo
  = HoverRow Int
  | NoHover

type State
  = { hover :: HoverInfo
    , entries :: Array Int
    }

data Action
  = SetHover HoverInfo

component :: forall q i o m. H.Component HH.HTML q i o m
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
      let
        active = case state.hover of
          HoverRow i
            | i == idx -> true
          _ -> false

        highlight
          -- | active = [ HP.classes [ B.tableActive ] ]
          | otherwise = []

        keyId
          | active = show idx <> "-active"
          | otherwise = show idx
      in
        HH.tr
          ( [ HE.onMouseEnter \_ -> Just (SetHover (HoverRow idx))
            ]
              <> highlight
          )
          $ map
              (\str -> HH.td_ [ HH.text str ])
              [ show idx, keyId ]
  in
    HH.table
      [ HP.classes [ B.table, B.tableSm, B.tableHover ]
      , HE.onMouseLeave \_ -> Just (SetHover NoHover)
      ]
      [ HH.thead_
          [ HH.tr_
              $ map
                  (\str -> HH.th [ HP.classes [ B.colSm1 ] ] [ HH.text str ])
                  [ "id", "keyId" ]
          ]
      , HH.tbody_ $ map mkRow state.entries
      ]

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  SetHover h -> H.modify_ \st -> st { hover = h }
