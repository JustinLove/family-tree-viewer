module View exposing (Msg(..), Mode(..), LayoutStatus(..), view, document)

import OHOLData.Decode as Decode exposing (Server)
import RemoteData exposing (RemoteData(..))
import LifeDataLayer

import Browser
import DatePicker
import Date as PickerDate
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (on)
import Http
import Json.Decode
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Time exposing (Posix)
import Url exposing (Url)
import Url.Builder as Url

type Msg
  = None
  | Search String
  | SearchTyping String
  | SelectServer Int
  | StartDateChange DatePicker.ChangeEvent
  | EndDateChange DatePicker.ChangeEvent
  | Back

type Mode
  = Query
  | Display

type LayoutStatus
  = LayoutRendering
  | LayoutIdle
  | LayoutError String

--document : (Msg -> msg) -> model -> Browser.Document msg
document tagger model =
  { title = "Family Tree Viewer"
  , body = [Html.map tagger (view model)]
  }

-- view : model -> Html Msg
view model = 
  layout
    [ Font.color foreground
    , Background.color background
    , inFront displayFooter
    , width fill
    ] <|
    case model.mode of
      Query -> query model
      Display -> display model

query model =
  column [ height fill, width fill ]
    [ searchBox model.searchTerm model.dataLayer.lives
    , serverSelect model.serverList model.selectedServer
    , dateSelect StartDateChange "Start Date" model.startDateModel
    , dateSelect EndDateChange "End Date" model.endDateModel
    , dateWarning model.startDateModel.date model.endDateModel.date
    , showResult model (LifeDataLayer.loadingCount model.dataLayer) model.lifeSearch.results
    ]

showResult model loading remote =
  case remote of
    NotRequested ->
      none
    NotAvailable ->
      none
    Loading ->
      el [ centerX, centerY ] <| text ("Loading " ++ (String.fromInt loading))
    Data lives ->
      showMatchingLives model lives
    Failed error ->
      showError error

showLoading : LayoutStatus -> RemoteData a -> Element Msg
showLoading layoutStatus remote =
  case remote of
    NotRequested ->
      none
    NotAvailable ->
      none
    Loading ->
      el [ centerX, centerY ] <| text "Loading"
    Data _ ->
      case layoutStatus of
        LayoutRendering -> el [ centerX, centerY ] <| text "Rendering"
        LayoutIdle -> none
        LayoutError message -> el [ centerX, centerY ] <| text ("Rendering Error: " ++ message)
    Failed error ->
      showError error

showError : Http.Error -> Element Msg
showError error =
  el [ centerX, centerY ] <|
    case error of
      Http.BadUrl url ->
        twoPartMessage 500
          "Bad Url"
          "This *really* shouldn't happen."
      Http.Timeout ->
        twoPartMessage 500
          "Timeout"
          "Wondible is cheap and you are getting this for free."
      Http.NetworkError ->
        twoPartMessage 500
          "Network Error"
          "Either you or the server went offline"
      Http.BadStatus code ->
        twoPartMessage 500
          (code |> String.fromInt)
          "Server is being naughty again.'"
      Http.BadBody body ->
        twoPartMessage 500
          "Bad Body"
          body

twoPartMessage : Int -> String -> String -> Element Msg
twoPartMessage height header body =
  column []
    [ el [ centerX, Font.size (scaled height 2)] <|
      text header
    , el [ centerX, Font.size (scaled height 1)] <|
      text body
    ]


showMatchingLives model lives =
  table [ spacing 10, padding 10, height fill, width fill, scrollbarY ]
    { data = lives
    , columns =
      [ { header = text "Name"
        , width = px 300
        , view = \life ->
          link []
            { url = displayUrl model.location life.serverId life.birthTime life.playerid
            , label = 
              life.name
              |> Maybe.withDefault "nameless"
              |> text
            }
        }
      , { header = text "Age"
        , width = px 40
        , view = \life ->
          life.age
          |> ceiling
          |> String.fromInt
          |> text
        }
      , { header = text "Born"
        , width = px 200
        , view = \life ->
          life.birthTime
          |> date model.zone
          |> text
        }
      , { header = text "Gen"
        , width = px 40
        , view = \life ->
          life.generation
          |> String.fromInt
          |> text
        }
      ]
    }

displayUrl : Url -> Int -> Posix -> Int -> String
displayUrl location serverId startTime playerid =
  { location
  | fragment =
    Url.toQuery
      [ Url.int "server_id" serverId
      , Url.int "start_time" ((Time.posixToMillis startTime) // 1000)
      , Url.int "playerid" playerid
      ]
      |> String.dropLeft 1
      |> Just
  } |> Url.toString

date : Time.Zone -> Posix -> String
date zone time =
  let
    year = Time.toYear zone time |> String.fromInt
    month = Time.toMonth zone time |> formatMonth
    day = Time.toDay zone time |> String.fromInt |> String.padLeft 2 '0'
    hour = Time.toHour zone time |> String.fromInt |> String.padLeft 2 '0'
    minute = Time.toMinute zone time |> String.fromInt |> String.padLeft 2 '0'
  in
    year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute

formatMonth : Time.Month -> String
formatMonth month =
  case month of
    Time.Jan -> "01"
    Time.Feb -> "02"
    Time.Mar -> "03"
    Time.Apr -> "04"
    Time.May -> "05"
    Time.Jun -> "06"
    Time.Jul -> "07"
    Time.Aug -> "08"
    Time.Sep -> "09"
    Time.Oct -> "10"
    Time.Nov -> "11"
    Time.Dec -> "12"

display model =
  Keyed.column
    [ width fill
    , height fill
    ]
    [ ("top-ui", row [ spacing 10, width fill ]
      [ searchBox model.searchTerm model.dataLayer.lives
      , Input.button []
        { onPress = Just Back
        , label = text "Back"
        }
      ])
    , ("loading", showLoading model.layoutStatus model.dataLayer.lives)
    , ("graph-container", Keyed.el [ width fill, height fill, clip, Background.color white ]
        ("graph", html <| svg
            [ Svg.Attributes.id "graph"
            , Svg.Attributes.width (String.fromInt model.windowWidth)
            , Svg.Attributes.height (String.fromInt model.windowHeight)
            ] []
        )
      )
    ]

searchBox : String -> RemoteData a -> Element Msg
searchBox searchTerm request =
  row [ padding 2, spacing 10, width fill ]
    [ Input.search
      [ htmlAttribute <| on "change" <| targetValue Json.Decode.string Search
      , padding 2
      , Background.color (if request == Loading then background else input)
      , width (px 400)
      , alignLeft
      ]
      { onChange = SearchTyping
      , text = searchTerm
      , placeholder = Nothing
      , label = Input.labelLeft [] (text "Character Name or Hash")
      }
    , Input.button [ alignLeft ]
      { onPress = Just (Search searchTerm)
      , label = text "Search"
      }
    , el [ width fill ] none
    ]

serverSelect : RemoteData (List Server) -> Maybe Int -> Element Msg
serverSelect servers serverId =
    Input.radioRow [ padding 10, spacing 2, htmlAttribute (Html.Attributes.class "server-select") ]
      { onChange = SelectServer
      , selected = serverId
      , label = Input.labelAbove [] (text "Server")
      , options = servers |> RemoteData.withDefault [] |> List.map serverItem
      }

serverItem : Server -> Input.Option Int Msg
serverItem server =
  Input.optionWith server.id
    (serverIcon server)

serverDisplayName : String -> String
serverDisplayName serverName =
  serverName
    |> String.split "."
    |> List.head
    |> Maybe.withDefault serverName

serverIcon : Server -> Input.OptionState -> Element Msg
serverIcon server =
  serverIconForName server.serverName

serverIconForName : String -> Input.OptionState -> Element Msg
serverIconForName serverName status =
  let
    name = serverDisplayName serverName
  in
  el [ htmlAttribute (Html.Attributes.title serverName) ] <|
  if String.startsWith "server" name then
    let
      number = String.replace "server" "" name
    in
      el
        [ width (px 30)
        , padding 3
        , Border.width 1
        , Border.color foreground
        , Border.rounded 8
        , Background.color (if status == Input.Selected then selected else control)
        ]
        (el [ centerX ] (text number))
  else if String.startsWith "bigserver" name then
    let
      number = String.replace "bigserver" "" name
    in
      el
        [ width (px 30)
        , Border.width 4
        , Border.color foreground
        , Border.rounded 8
        , Font.heavy
        , Font.color (if status == Input.Selected then background else foreground)
        , Background.color (if status == Input.Selected then selected else control)
        ]
        (el [ centerX ] (text number))
  else
    text name

dateSelect msg label model =
  DatePicker.input
    [ width (fill |> maximum 400)
    , padding 2
    , Background.color input
    ]
    { onChange = msg
    , selected = model.date
    , text = model.text
    , label =
        Input.labelAbove [] <|
            Element.text label
    , placeholder = Nothing
    , settings = pickerSettings
    , model = model.picker
    }

defaultPickerSettings = DatePicker.defaultSettings  

pickerSettings : DatePicker.Settings
pickerSettings =
  { defaultPickerSettings
  | firstDayOfWeek = Time.Sun
  , pickerAttributes =
      [ Border.width 1
      , Border.color (Element.rgb255 186 189 182)
      , Border.roundEach
          { topLeft = 0
          , topRight = 0
          , bottomLeft = 3
          , bottomRight = 3
          }
      , Element.moveDown 3
      , padding 8
      , spacing 4
      , Element.centerX
      , Element.centerY
      , Element.width Element.fill
      , Background.color background
      ]
  }

dateWarning : Maybe PickerDate.Date -> Maybe PickerDate.Date -> Element msg
dateWarning start end =
  case Maybe.map2 (PickerDate.diff PickerDate.Days) start end of
    Just count ->
      let countText = (String.fromInt count) in
      if count < 14 then
        none
      else if count < 60 then
        el [ Font.color caution ] (text "Large queries may make your browser slow or unstable")
      else if count < 365 then
        el [ Font.color warning ] (text "Very large queries will likely make your browser slow or unstable")
      else
        el [ Font.color warning ] (text "Even if your browser can handle it, you are limited to one year")
    Nothing -> none

displayFooter : Element msg
displayFooter =
  row
    [ Region.footer
    , spacing 10
    , padding 2
    , alignBottom
    , Font.size (scaled 500 -2)
    , Background.color background
    ]
    [ link []
      { url = "https://github.com/JustinLove/family-trees-viewer"
      , label = row [] [ icon "github", text "family-trees-viewer" ]
      }
    , link []
      { url = "https://github.com/JustinLove/ohol-data-server"
      , label = row [] [ icon "github", text "ohol-data-server" ]
      }
    , link []
      { url = "https://github.com/JustinLove/ohol-family-trees"
      , label = row [] [ icon "github", text "ohol-family-trees" ]
      }
    , link []
      { url = "https://twitter.com/wondible"
      , label = row [] [ icon "twitter", text "@wondible" ]
      }
    ]

icon : String -> Element msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]
  |> html

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)

scaled height = modular (max ((toFloat height)/30) 15) 1.25 >> round

foreground = rgb 0.9 0.9 0.9
background = rgb 0.1 0.1 0.1
white = rgb 1 1 1
selected = rgb 0.23 0.6 0.98
control = rgb 0.2 0.2 0.2
input = rgb 0 0 0
caution = rgb 0.8 0.62 0.0
warning = rgb 0.95 0.05 0.03
