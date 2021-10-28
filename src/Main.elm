module Main exposing (main)

import Browser exposing (element)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error, get, expectString)
import Json.Decode exposing (map3, field, string, decodeString)
import Date exposing (Date, Interval(..), range, fromCalendarDate, fromPosix, toIsoString)
import Time exposing (Month(..), Posix, utc, now)
import Task exposing (perform)


-- 📦 model 📦


type alias Data =
    { patients: String
    , covid: String
    , beds: String
    }

type Status
    = Failure
    | Loading
    | Success


type alias Query =
    { state : String
    , date : String
    }


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
        (
            { status = Loading
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


type alias Response = Result Error String


type Msg
    = Fetch Query
    | Receive Response
    | RegisterToday Posix



-- 📩 -------- 📩
--
--
-- 🤝 helpers 🤝


defaultData : Data
defaultData = Data "" "" ""


field : String -> Json.Decode.Decoder String
field key = Json.Decode.field key string


decodeData : String -> Result Json.Decode.Error (List Data)
decodeData =
    decodeString <| Json.Decode.list <| map3 Data
        (field "inpatient_beds_used")
        (field "inpatient_beds_used_covid")
        (field "inpatient_beds")

fetchAPI : Query -> Cmd Msg
fetchAPI query =
    get
        { url = "https://healthdata.gov/resource/g62h-syeh.json?"
            ++ "date=" ++ query.date
            ++ "&state=" ++ query.state
        , expect = expectString Receive
        }


fail : Model -> Model
fail model = { model | status = Failure }



states : List String
states = [ "AL", "AK", "AS", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VI", "VA", "WA", "WV", "WI", "WY" ]


type alias DropdownProps =
    { model : Model
    , getQuery : Query -> String
    , generateQuery : String -> Query
    , options : List String
    , id : String
    , label : String
    }


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
        <| List.map
            ( \label -> option [] [ text label ] )
            props.options
    ]


dates : Date -> List String
dates today = List.reverse <| List.map toIsoString
    ( range Day 1
        ( fromCalendarDate 2021 Jan 1 )
        today
    )
    

statistic : String -> String -> Html msg
statistic label dataFigure =
    li []
        [ span
            [ class "label" ]
            [ text <| label ++ ": " ]
        , span
            [ class "stat"]
            [ text dataFigure ]
        ]


-- 🤝 ------- 🤝
--
--
-- 📃 view 📃


view : Model -> Html Msg
view model =
    let
        query = model.query
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
                    Loading -> p [] [ text "Loading 🔄️" ]
                
                    Failure -> p [] [ text "Data unavailable 😢" ]
                
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
            ( case response of
                Err _ -> fail model
                
                Ok contents ->
                    case decodeData contents of
                        Err _ -> fail model
                        
                        Ok dataList ->
                            case List.head dataList of
                                Nothing -> fail model
                                    
                                Just data ->
                                    { model
                                        | status = Success
                                        , data = data
                                    }
            , Cmd.none
            )
                    
        RegisterToday moment ->
            ( { model | today = fromPosix utc moment }
            , Cmd.none
            )



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
