-- here's a sample app for you! :D


module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- 📦 model 📦


type alias Model =
    { weekday : String
    , nightMode : Bool
    }


init : String -> Model
init weekday =
    { weekday = weekday
    , nightMode = False
    }



-- 📦 ----- 📦
--
--
-- 📩 messages 📩


type Msg = ToggleNightMode



-- 📩 -------- 📩
--
--
-- 🤝 helpers 🤝


darkStyle : Bool -> List (Attribute msg)
darkStyle active = if active then [ class "dark" ] else []


toggleIcon : Bool -> Html msg
toggleIcon active = text ( if active then "light_mode" else "dark_mode" )



-- 🤝 ------- 🤝
--
--
-- 📃 view 📃


view : Model -> Html Msg
view model =
    section (id "greeting" :: darkStyle model.nightMode)
        [ div [ class "card" ]
            [ h1 [] [ text "🎈 it works!" ]
            , p [] [ text ( "have a lovely " ++ model.weekday ++ "." ) ]
            , button
                [ id "night-mode-toggle", class "material-icons-round", onClick ToggleNightMode ]
                [ toggleIcon model.nightMode ]
            ]
        ]



-- 📃 ---- 📃
--
--
-- 📯 update 📯


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleNightMode -> { model | nightMode = not model.nightMode }



-- 📯 ------ 📯
--
--
-- 💻 main 💻


main : Program String Model Msg
main =
    Browser.element
        { init = \flags -> ( init flags, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- 💻 ---- 💻
