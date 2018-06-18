import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Random exposing (..)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Model =
  { gameId : String
  , game : Game
  }

emptyModel : Model
emptyModel = { gameId = "" , game = { thumbnail = "" , name = "" } }

init : (Model, Cmd Msg)
init =
  ( emptyModel
  , moreGames
  )



-- UPDATE

type Msg
  = MoreGames
  | NewGame Int
  | GetGame (Result Http.Error Game)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MoreGames ->
      (model, Random.generate NewGame (Random.int 1 223000))

    GetGame (Ok newUrl) ->
      (Model model.gameId newUrl, Cmd.none)

    GetGame (Err _) ->
      (model, Cmd.none)

    NewGame gameId ->
      (Model (toString gameId) model.game, getGame (toString gameId))

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.game.name]
    , br [] []
    , img [src model.game.thumbnail] []
    , br [] []
    , br [] []
    , button [ onClick MoreGames ] [ text "More Games!" ]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP

type alias Game = { name: String, thumbnail: String}

getGame : String -> Cmd Msg
getGame gameId =
  let
    url =
      "https://bgg-json.azurewebsites.net/thing/" ++ gameId
  in
    Http.send GetGame (Http.get url gameDecoder)


gameDecoder = Decode.map2 Game (Decode.field "name" Decode.string) (Decode.field "thumbnail" Decode.string)

moreGames: Cmd Msg
moreGames = Cmd.map ( always MoreGames) Cmd.none