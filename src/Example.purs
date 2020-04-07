module Example where

import Prelude
import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Themes.Bootstrap4 as B

tableHooks :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
tableHooks =
  Hooks.component \_ -> Hooks.do
    hover /\ hoverState <- Hooks.useState NoHover
    entries /\ entriesState <- Hooks.useState $ 1 .. 1000
    let
      mkRow idx =
        HH.tr
          [ HE.onMouseEnter \_ -> --do
              --log $ show idx
              Just $ Hooks.put hoverState $ HoverRow idx
          ]
          $ map
              (\str -> HH.td_ [ HH.text str ])
              [ show idx, show $ idx * 10 ]
    Hooks.pure do
      let
        hovTxt = case hover of
          HoverRow i -> show i
          NoHover -> "Not hovering"
      HK.div [ HP.class_ B.col ]
        [ Tuple hovTxt
            $ HH.div_
                [ HH.text hovTxt
                ]
        , Tuple "same"
            $ HH.table
                [ HP.classes [ B.table, B.tableSm, B.tableHover ] ]
                [ HH.thead_
                    [ HH.tr_
                        $ map
                            (\str -> HH.th [ HP.classes [ B.colSm1 ] ] [ HH.text str ])
                            [ "id", "value" ]
                    ]
                , HH.tbody
                    [ HE.onMouseLeave \_ -> --do
                        --log $ "exit"
                        Just $ Hooks.put hoverState NoHover
                    ]
                    $ map mkRow entries
                ]
        ]

data HoverInfo
  = HoverRow Int
  | NoHover
