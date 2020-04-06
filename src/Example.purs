module Example where

import Prelude
import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Themes.Bootstrap4 as B
import Halogen.HTML.Properties as HP

type State
  = { entries :: Array Int
    }

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const { entries: 1 .. 3 }
    , render
    , eval: H.mkEval $ H.defaultEval
    }

render :: forall a m. State -> H.ComponentHTML a () m
render state =
  let
    mkRow idx =
      HH.tr_
        $ map
            (\str -> HH.td_ [ HH.text str ])
            [ show idx, show $ idx * 10 ]
  in
    HH.table
      [ HP.classes [ B.table, B.tableSm, B.tableHover ]
      ]
      $ [ HH.tr_
            $ map
                (\str -> HH.th_ [ HH.text str ])
                [ "id", "value" ]
        ]
      <> map mkRow state.entries
