module Example where

import Prelude
import CSS (backgroundColor, grey)
import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML.CSS as CSS
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data HoverInfo
  = HoverRow Int
  | NoHover

type State
  = { hover :: HoverInfo }

data Action
  = SetHover HoverInfo

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const { hover: NoHover }
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
          | active = [ CSS.style $ backgroundColor grey ]
          | otherwise = []
      in
        HH.tr
          ( [ HE.onMouseEnter \_ -> Just (SetHover (HoverRow idx))
            ]
              <> highlight
          )
          [ HH.td_ [ HH.text $ show idx ] ]
  in
    HH.table
      [ HE.onMouseLeave \_ -> Just (SetHover NoHover)
      ]
      $ [ HH.tr_ [ HH.th_ [ HH.text "id" ] ]
        ]
      <> map mkRow (1 .. 1000)

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  SetHover h -> H.modify_ \st -> st { hover = h }
