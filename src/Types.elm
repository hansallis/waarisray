module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Lamdera exposing (ClientId, SessionId)
import Time
import Url exposing (Url)



-- CORE TYPES


type alias Location =
    { lat : Float
    , lng : Float
    }


type alias TelegramUser =
    { id : Int
    , firstName : String
    , lastName : Maybe String
    , username : Maybe String
    , photoUrl : Maybe String
    }


type alias User =
    { telegramUser : TelegramUser
    , isRay : Bool -- Special flag for Ray (the pilot)
    }


type alias Guess =
    { userId : Int
    , userName : String
    , location : Location
    , timestamp : Time.Posix
    , distanceKm : Maybe Float -- Calculated after round closes
    }


type alias CensoredGuess =
    { userId : Int
    , userName : String
    , location : Maybe Location
    , timestamp : Time.Posix
    }


type alias CensoredRound =
    Round () () CensoredGuess


type alias UncensoredRound =
    Round String Location Guess


type FrontendRound
    = Uncensored UncensoredRound
    | Censored CensoredRound


toCensoredRound : FrontendRound -> CensoredRound
toCensoredRound round =
    case round of
        Censored rnd ->
            rnd

        Uncensored rnd ->
            censorRound rnd


censorRound : UncensoredRound -> CensoredRound
censorRound { actualLocation, startTime, endTime, guesses, isOpen } =
    { actualLocation = ()
    , prompt = ()
    , startTime = startTime
    , endTime = endTime
    , guesses = Dict.map (always (\{ userId, userName, location, timestamp } -> { userId = userId, userName = userName, location = Just location, timestamp = timestamp })) guesses
    , isOpen = isOpen
    }


fullyCensorRound : UncensoredRound -> CensoredRound
fullyCensorRound =
    censorRound >> (\round -> { round | guesses = Dict.map (always (\guess -> { guess | location = Nothing })) round.guesses })


type alias Round promptType actualLocationType guessType =
    { actualLocation : actualLocationType
    , prompt : promptType
    , startTime : Time.Posix
    , endTime : Maybe Time.Posix
    , guesses : Dict Int guessType -- userId -> Guess
    , isOpen : Bool
    }


type RoundStatus
    = WaitingForLocation -- Ray needs to set location
    | Open -- Users can make guesses
    | Closed -- Round finished, showing results


type alias ImageGenerationParams =
    { numberOfImages : Int
    , guidanceScale : Float
    , seed : Maybe Int
    }


type Page
    = LoginPage
    | GamePage
    | ResultsPage Int -- Round Index
    | HistoryPage



-- FRONTEND MODEL


type alias FrontendModel =
    { key : Key
    , currentUser : Maybe User
    , page : Page
    , currentRound : Maybe FrontendRound
    , pastRounds : List UncensoredRound
    , userGuess : Maybe Location
    , pendingLocation : Maybe Location -- Ray's pending location before confirming
    , prompt : String
    , mapCenter : Location
    , mapZoom : Int
    , avatarList : List ( Int, String )
    , error : Maybe String
    , authData : Maybe String -- Telegram auth data
    }



-- BACKEND MODEL


type alias BackendModel =
    { users : Dict Int User -- userId -> User
    , rounds : List UncensoredRound -- roundId -> Round
    , userSessions : Dict SessionId Int -- sessionId -> userId
    , failedAuthentications : List String
    , now : Time.Posix
    }



-- FRONTEND MESSAGES


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | -- Authentication
      InitiateTelegramAuth
    | TelegramAuthResult (Result Http.Error String)
    | Logout
    | -- Testing authentication
      LoginAsRay
    | LoginAsRegularUser
    | -- Map interactions
      MapClicked Location
    | -- Game actions
      StartNewRound
    | ConfirmStartRound
    | CancelPendingLocation
    | SubmitGuess
    | CloseRound
    | ViewRound Int
    | GoToHistory
    | GoToGame
    | PromptChanged String
    | -- UI
      ClearError
    | NoOpFrontendMsg



-- BACKEND MESSAGES


type BackendMsg
    = NoOpBackendMsg
    | Tick Time.Posix
    | GotImageGenerationResult String (Result Http.Error { images : List String, error : Maybe String }) -- userName, result



-- FRONTEND -> BACKEND


type ToBackend
    = -- Authentication
      AuthenticateWithTelegram String -- auth data
    | LogoutUser
    | -- Testing authentication
      AuthenticateAsRay
    | AuthenticateAsRegularUser
    | -- Game actions
      CreateNewRound Location String -- Ray sets location and image prompt
    | SubmitUserGuess Location
    | EndCurrentRound
    | RequestCurrentGameState
    | RequestRoundHistory



-- BACKEND -> FRONTEND


type ToFrontend
    = -- Authentication
      AuthenticationResult (Result String User)
    | -- Game state updates
      GameStateUpdate
        { currentUser : Maybe User
        , currentRound : Maybe FrontendRound
        , usersGuess : Maybe Location
        , pastRounds : List UncensoredRound
        , avatarList : List ( Int, String )
        }
    | RoundCreated CensoredRound
    | GuessSubmitted Guess -- Full guess with all info
    | RoundClosed UncensoredRound -- Round with calculated distances
    | -- Errors
      ErrorMessage String



-- HELPER FUNCTIONS


initialLocation : Location
initialLocation =
    { lat = 52.344006, lng = 5.626099 }



-- London


calculateDistance : Location -> Location -> Float
calculateDistance loc1 loc2 =
    let
        toRadians degrees =
            degrees * pi / 180

        lat1 =
            toRadians loc1.lat

        lng1 =
            toRadians loc1.lng

        lat2 =
            toRadians loc2.lat

        lng2 =
            toRadians loc2.lng

        dlat =
            lat2 - lat1

        dlng =
            lng2 - lng1

        a =
            sin (dlat / 2) ^ 2 + cos lat1 * cos lat2 * sin (dlng / 2) ^ 2

        c =
            2 * atan2 (sqrt a) (sqrt (1 - a))

        r =
            6371

        -- Earth's radius in kilometers
    in
    r * c


encodeLocation : Location -> Encode.Value
encodeLocation location =
    Encode.object
        [ ( "lat", Encode.float location.lat )
        , ( "lng", Encode.float location.lng )
        ]


locationDecoder : Decode.Decoder Location
locationDecoder =
    Decode.map2 Location
        (Decode.field "lat" Decode.float)
        (Decode.field "lng" Decode.float)


encodeTelegramUser : TelegramUser -> Encode.Value
encodeTelegramUser user =
    Encode.object
        [ ( "id", Encode.int user.id )
        , ( "firstName", Encode.string user.firstName )
        , ( "lastName", encodeNullable Encode.string user.lastName )
        , ( "username", encodeNullable Encode.string user.username )
        , ( "photoUrl", encodeNullable Encode.string user.photoUrl )
        ]


telegramUserDecoder : Decode.Decoder TelegramUser
telegramUserDecoder =
    Decode.map5 TelegramUser
        (Decode.field "id" Decode.int)
        (Decode.field "first_name" Decode.string)
        (Decode.maybe (Decode.field "last_name" Decode.string))
        (Decode.maybe (Decode.field "username" Decode.string))
        (Decode.maybe (Decode.field "photo_url" Decode.string))


encodeNullable : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeNullable encoder maybeValue =
    case maybeValue of
        Just value ->
            encoder value

        Nothing ->
            Encode.null
