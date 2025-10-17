module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Http
import Lamdera
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
    | ResultsPage String
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


type alias Round =
    { id : String
    , createdBy : Int
    , actualLocation : Location
    , startTime : Time.Posix
    , endTime : Maybe Time.Posix
    , guesses : Dict.Dict Int Guess
    , isOpen : Bool
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , currentUser : Maybe User
    , page : Page
    , currentRound : Maybe Round
    , pastRounds : List Round
    , userGuess : Maybe Location
    , pendingLocation : Maybe Location
    , showingGuesses : Bool
    , mapCenter : Location
    , mapZoom : Int
    , error : Maybe String
    , authData : Maybe String
    }


type alias BackendModel =
    { users : Dict.Dict Int User
    , rounds : Dict.Dict String Round
    , currentRoundId : Maybe String
    , userSessions : Dict.Dict Lamdera.SessionId Int
    , sessionClients : Dict.Dict Lamdera.SessionId (List Lamdera.ClientId)
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
    | SetMapCenter Location Float
    | StartNewRound
    | ConfirmStartRound
    | CancelPendingLocation
    | SubmitGuess
    | CloseRound
    | ViewRound String
    | GoToHistory
    | GoToGame
    | ToggleGuessesVisibility
    | ClearError
    | NoOpFrontendMsg


type ToBackend
    = AuthenticateWithTelegram String
    | LogoutUser
    | AuthenticateAsRay
    | AuthenticateAsRegularUser
    | CreateNewRound Location
    | SubmitUserGuess Location
    | EndCurrentRound
    | RequestCurrentGameState
    | RequestRoundHistory


type BackendMsg
    = NoOpBackendMsg
    | Connected Lamdera.SessionId Lamdera.ClientId
    | Disconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = AuthenticationResult (Result String User)
    | GameStateUpdate
        { currentUser : Maybe User
        , currentRound : Maybe Round
        , pastRounds : List Round
        }
    | RoundCreated Round
    | GuessSubmitted Guess
    | RoundClosed Round
    | ErrorMessage String
