-- sample app too clunky? here's the template :P


-- change to "Main" if using
module Template exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- 📦 model 📦


type alias Model = { todo: Int {- implement model -} }


init: String -> ( Model, Cmd msg )
init flags = ( { todo = Debug.todo "implement init" }, Cmd.none )



-- 📦 ----- 📦
--
--
-- 📩 messages 📩


type Msg = Todo



-- 📩 -------- 📩
--
--
-- 🤝 helpers 🤝


helper input = Ok



-- 🤝 ------- 🤝
--
--
-- 📃 view 📃


view model = Debug.todo "implement view"



-- 📃 ---- 📃
--
--
-- 📯 update 📯


update msg model =
    ( case msg of
        Todo -> { model | todo = Debug.todo "implement update" }
    , Cmd.none
    )



-- 📯 ------ 📯
--
--
-- 💻 main 💻

main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- 💻 ---- 💻
