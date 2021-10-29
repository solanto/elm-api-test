module Main exposing (main)

import Browser exposing (element)
import Date exposing (Date, Interval(..), fromCalendarDate, fromPosix, range, toIsoString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error, expectString, get)
import Json.Decode exposing (decodeString, field, map3, string)
import Maybe exposing (andThen)
import Result exposing (toMaybe)
import Task exposing (perform)
import Time exposing (Month(..), Posix, now, utc)



-- 📦 model 📦


type alias Model =
    { status : Status
    , query : Query
    , data : Data
    , today : Date
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        query =
            { state = "MD"
            , date = "2021-10-01"
            }
    in
    ( { status = Loading
      , query = query
      , today = fromCalendarDate 2021 Oct 1
      , data = defaultData
      }
    , Cmd.batch
        [ fetchAPI query
        , perform RegisterToday now
        ]
    )



-- 📦 ----- 📦
--
--
-- 📩 messages 📩


type Msg
    = Fetch Query
    | Receive Response
    | RegisterToday Posix



-- 📩 -------- 📩
--
--
-- 🧩 types 🧩


type alias Data =
    { patients : String
    , covid : String
    , beds : String
    }


type alias DropdownProps =
    { model : Model
    , getQuery : Query -> String
    , generateQuery : String -> Query
    , options : List String
    , id : String
    , label : String
    }


type alias Query =
    { state : String
    , date : String
    }


type alias Response =
    Result Error String


type Status
    = Failure
    | Loading
    | Success



-- 🧩 ----- 🧩
--
--
-- 🤝 helpers 🤝


defaultData : Data
defaultData =
    Data "" "" ""


field : String -> Json.Decode.Decoder String
field key =
    Json.Decode.field key string


decodeData : String -> Result Json.Decode.Error (List Data)
decodeData =
    decodeString <|
        Json.Decode.list <|
            map3 Data
                (field "inpatient_beds_used")
                (field "inpatient_beds_used_covid")
                (field "inpatient_beds")


fetchAPI : Query -> Cmd Msg
fetchAPI query =
    get
        { url =
            "https://healthdata.gov/resource/g62h-syeh.json?"
                ++ "date="
                ++ query.date
                ++ "&state="
                ++ query.state
        , expect = expectString Receive
        }


states : List String
states =
    [ "AL", "AK", "AS", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VI", "VA", "WA", "WV", "WI", "WY" ]


queryDropdown : DropdownProps -> Html Msg
queryDropdown props =
    div []
        [ label [ for props.id, class "label" ] [ text props.label ]
        , select
            [ id props.id
            , class "stat"
            , value <| props.getQuery props.model.query
            , onInput <| \option -> Fetch <| props.generateQuery option
            ]
          <|
            List.map
                (\label -> option [] [ text label ])
                props.options
        ]


dates : Date -> List String
dates today =
    List.reverse <|
        List.map toIsoString
            (range Day
                1
                (fromCalendarDate 2020 Jan 1)
                today
            )


statistic : String -> String -> Html msg
statistic label dataFigure =
    li []
        [ span
            [ class "label" ]
            [ text <| label ++ ": " ]
        , span
            [ class "stat" ]
            [ text dataFigure ]
        ]


getData : Response -> Maybe Data
getData response =
    toMaybe response
        |> andThen (toMaybe << decodeData)
        |> andThen List.head


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


info : String -> String -> Html msg
info className message = aside
    [ class "info"
    , class className
    ]
    [ text message ]



-- 🤝 ------- 🤝
--
--
-- 📃 view 📃


view : Model -> Html Msg
view model =
    let
        query =
            model.query
    in
    article []
        [ h1 [] [ text "COVID-19 Hospitalization Data" ]
        , Html.form []
            [ queryDropdown
                { model = model
                , getQuery = .state
                , generateQuery = \state -> { query | state = state }
                , options = states
                , id = "state"
                , label = "State"
                }
            , queryDropdown
                { model = model
                , getQuery = .date
                , generateQuery = \date -> { query | date = date }
                , options = dates model.today
                , id = "date"
                , label = "Date"
                }
            ]
        , case model.status of
            Loading -> info "loading" "Loading"

            Failure -> info "failure" "Data unavailable"

            Success ->
                ul []
                    [ statistic "Total patients" model.data.patients
                    , statistic "COVID patients" model.data.covid
                    , statistic "Beds" model.data.beds
                    ]
        ]



-- 📃 ---- 📃
--
--
-- 📯 update 📯


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch query ->
            ( { model | query = query, status = Loading }
            , fetchAPI query
            )

        Receive response ->
            noCmd <|
                case getData response of
                    Nothing ->
                        { model | status = Failure }

                    Just data ->
                        { model
                            | status = Success
                            , data = data
                        }

        RegisterToday moment ->
            noCmd { model | today = fromPosix utc moment }



-- 📯 ------ 📯
--
--
-- 💻 main 💻


main : Program () Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- 💻 ---- 💻
