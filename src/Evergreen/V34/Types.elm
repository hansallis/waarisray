module Evergreen.V34.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Http
import Lamdera
import Random
import Time
import Url


type alias TelegramUser =
    { id : Int
    , firstName : String
    , lastName : Maybe String
    , username : Maybe String
    , photoUrl : Maybe String
    }


type alias User =
    { telegramUser : TelegramUser
    , isRay : Bool
    }


type Page
    = LoginPage
    | GamePage
    | ResultsPage Int
    | HistoryPage


type alias Location =
    { lat : Float
    , lng : Float
    }


type alias Guess =
    { userId : Int
    , userName : String
    , location : Location
    , timestamp : Time.Posix
    , distanceKm : Maybe Float
    }


type alias Round promptType actualLocationType guessType =
    { actualLocation : actualLocationType
    , prompt : promptType
    , startTime : Time.Posix
    , endTime : Maybe Time.Posix
    , guesses : Dict.Dict Int guessType
    , isOpen : Bool
    }


type alias UncensoredRound =
    Round String Location Guess


type alias CensoredGuess =
    { userId : Int
    , userName : String
    , location : Maybe Location
    , timestamp : Time.Posix
    }


type alias CensoredRound =
    Round () () CensoredGuess


type FrontendRound
    = Uncensored UncensoredRound
    | Censored CensoredRound


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , currentUser : Maybe User
    , page : Page
    , currentRound : Maybe FrontendRound
    , pastRounds : List UncensoredRound
    , userGuess : Maybe Location
    , pendingLocation : Maybe Location
    , prompt : String
    , mapCenter : Location
    , mapZoom : Int
    , avatarList : List ( Int, String )
    , error : Maybe String
    , authData : Maybe String
    }


type alias BackendModel =
    { users : Dict.Dict Int User
    , rounds : List UncensoredRound
    , userSessions : Dict.Dict Lamdera.SessionId Int
    , failedAuthentications : List String
    , now : Time.Posix
    , seed : Random.Seed
    , evergreenTrigger : List ()
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | InitiateTelegramAuth
    | TelegramAuthResult (Result Http.Error String)
    | Logout
    | LoginAsRay
    | LoginAsRegularUser
    | MapClicked Location
    | StartNewRound
    | ConfirmStartRound
    | CancelPendingLocation
    | SubmitGuess
    | CloseRound
    | ViewRound Int
    | GoToHistory
    | GoToGame
    | PromptChanged String
    | ClearError
    | NoOpFrontendMsg


type ToBackend
    = AuthenticateWithTelegram String
    | LogoutUser
    | AuthenticateAsRay
    | AuthenticateAsRegularUser
    | CreateNewRound Location String
    | SubmitUserGuess Location String
    | EndCurrentRound
    | RequestCurrentGameState
    | RequestRoundHistory


type BackendMsg
    = NoOpBackendMsg
    | Tick Time.Posix
    | GotImageGenerationResult
        String
        (Result
            ( Http.Error, String )
            { images : List String
            , error : Maybe String
            }
        )
    | GotGeocodeResult String String (Result Http.Error String)


type ToFrontend
    = AuthenticationResult (Result String User)
    | GameStateUpdate
        { currentUser : Maybe User
        , currentRound : Maybe FrontendRound
        , usersGuess : Maybe Location
        , pastRounds : List UncensoredRound
        , avatarList : List ( Int, String )
        }
    | RoundCreated CensoredRound
    | GuessSubmitted Guess
    | RoundClosed UncensoredRound
    | ErrorMessage String
