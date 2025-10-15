module Backend exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Lamdera exposing (ClientId, SessionId, onConnect, onDisconnect, sendToFrontend)
import Random
import Task
import Time
import Types exposing (..)


-- MODEL

type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { users = Dict.empty
      , rounds = Dict.empty
      , currentRoundId = Nothing
      , userSessions = Dict.empty
      , sessionClients = Dict.empty
      }
    , Cmd.none
    )


-- UPDATE

update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        Connected sessionId clientId ->
            let
                updated =
                    case Dict.get sessionId model.sessionClients of
                        Just clients ->
                            Dict.insert sessionId (clientId :: clients) model.sessionClients

                        Nothing ->
                            Dict.insert sessionId [ clientId ] model.sessionClients
            in
            ( { model | sessionClients = updated }, Cmd.none )

        Disconnected sessionId clientId ->
            let
                updated =
                    case Dict.get sessionId model.sessionClients of
                        Just clients ->
                            let
                                remaining = List.filter ((/=) clientId) clients
                            in
                            if List.isEmpty remaining then
                                Dict.remove sessionId model.sessionClients
                            else
                                Dict.insert sessionId remaining model.sessionClients

                        Nothing ->
                            model.sessionClients
            in
            ( { model | sessionClients = updated }, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        AuthenticateWithTelegram authData ->
            handleTelegramAuth sessionId clientId authData model

        LogoutUser ->
            handleLogout sessionId clientId model

        AuthenticateAsRay ->
            handleTestAuth sessionId clientId True model

        AuthenticateAsRegularUser ->
            handleTestAuth sessionId clientId False model

        CreateNewRound location ->
            handleCreateNewRound sessionId clientId location model

        SubmitUserGuess location ->
            handleSubmitGuess sessionId clientId location model

        EndCurrentRound ->
            handleEndRound sessionId clientId model

        RequestCurrentGameState ->
            handleRequestGameState sessionId clientId model

        RequestRoundHistory ->
            handleRequestRoundHistory sessionId clientId model


-- AUTHENTICATION

handleTelegramAuth : SessionId -> ClientId -> String -> Model -> ( Model, Cmd BackendMsg )
handleTelegramAuth sessionId clientId authData model =
    case decodeTelegramAuth authData of
        Ok telegramUser ->
            let
                -- Ray's Telegram ID - you'll need to replace this with Ray's actual ID
                rayTelegramId = 123456789  -- Replace with Ray's actual Telegram ID

                user =
                    { telegramUser = telegramUser
                    , isRay = telegramUser.id == rayTelegramId
                    }

                newModel =
                    { model
                    | users = Dict.insert telegramUser.id user model.users
                    , userSessions = Dict.insert sessionId telegramUser.id model.userSessions
                    }
                
                _ = Debug.log "✅ Authentication Success" 
                    { sessionId = sessionId
                    , userId = telegramUser.id
                    , userName = telegramUser.firstName
                    , isRay = user.isRay
                    }
            in
            ( newModel
            , sendToFrontend clientId (AuthenticationResult (Ok user))
            )

        Err error ->
            ( model
            , sendToFrontend clientId (AuthenticationResult (Err error))
            )


handleLogout : SessionId -> ClientId -> Model -> ( Model, Cmd BackendMsg )
handleLogout sessionId clientId model =
    let
        _ = Debug.log "🚪 Logout" { sessionId = sessionId }
        
        newModel =
            { model | userSessions = Dict.remove sessionId model.userSessions }
            
        gameState =
            { currentUser = Nothing
            , currentRound = Nothing
            , pastRounds = []
            }
    in
    ( newModel
    , sendToFrontend clientId (GameStateUpdate gameState)
    )


handleTestAuth : SessionId -> ClientId -> Bool -> Model -> ( Model, Cmd BackendMsg )
handleTestAuth sessionId clientId isRay model =
    let
        testUser =
            if isRay then
                { telegramUser = 
                    { id = 123456789  -- Ray's test ID
                    , firstName = "Ray"
                    , lastName = Just "Pilot"
                    , username = Just "raypilot"
                    , photoUrl = Nothing
                    }
                , isRay = True
                }
            else
                { telegramUser = 
                    { id = 987654321  -- Regular user test ID
                    , firstName = "Test"
                    , lastName = Just "User"
                    , username = Just "testuser"
                    , photoUrl = Nothing
                    }
                , isRay = False
                }

        newModel =
            { model
            | users = Dict.insert testUser.telegramUser.id testUser model.users
            , userSessions = Dict.insert sessionId testUser.telegramUser.id model.userSessions
            }
        
        _ = Debug.log "🧪 Test Authentication" 
            { sessionId = sessionId
            , userId = testUser.telegramUser.id
            , userName = testUser.telegramUser.firstName
            , isRay = testUser.isRay
            }
    in
    ( newModel
    , sendToFrontend clientId (AuthenticationResult (Ok testUser))
    )


decodeTelegramAuth : String -> Result String TelegramUser
decodeTelegramAuth authData =
    -- In a real implementation, you would validate the Telegram auth data
    -- using the bot token and check the hash. For now, we'll decode the basic data.
    case Decode.decodeString telegramUserDecoder authData of
        Ok user ->
            Ok user
        Err _ ->
            Err "Invalid authentication data"


-- ROUND MANAGEMENT

handleCreateNewRound : SessionId -> ClientId -> Location -> Model -> ( Model, Cmd BackendMsg )
handleCreateNewRound sessionId clientId location model =
    case getUserFromSession sessionId model of
        Just user ->
            if user.isRay then
                let
                    roundId = generateRoundId ()
                    
                    newRound =
                        { id = roundId
                        , createdBy = user.telegramUser.id
                        , actualLocation = location
                        , startTime = Time.millisToPosix 0  -- Will be set properly with Time.now
                        , endTime = Nothing
                        , guesses = Dict.empty
                        , isOpen = True
                        }

                    newModel =
                        { model
                        | rounds = Dict.insert roundId newRound model.rounds
                        , currentRoundId = Just roundId
                        }
                in
                ( newModel
                , Cmd.batch
                    [ Task.perform (always NoOpBackendMsg) (Task.succeed ())
                    , broadcastRoundCreated newRound model
                    ]
                )
            else
                ( model
                , sendToFrontend clientId (ErrorMessage "Only Ray can create rounds")
                )

        Nothing ->
            ( model
            , sendToFrontend clientId (ErrorMessage "Authentication required")
            )


handleSubmitGuess : SessionId -> ClientId -> Location -> Model -> ( Model, Cmd BackendMsg )
handleSubmitGuess sessionId clientId location model =
    case (getUserFromSession sessionId model, model.currentRoundId) of
        (Just user, Just roundId) ->
            if user.isRay then
                ( model
                , sendToFrontend clientId (ErrorMessage "Ray cannot submit guesses")
                )
            else
                case Dict.get roundId model.rounds of
                    Just round ->
                        if round.isOpen then
                            if Dict.member user.telegramUser.id round.guesses then
                                ( model
                                , sendToFrontend clientId (ErrorMessage "You have already submitted a guess for this round")
                                )
                            else
                                let
                                    guess =
                                        { userId = user.telegramUser.id
                                        , location = location
                                        , timestamp = Time.millisToPosix 0  -- Will be set properly
                                        , distanceKm = Nothing  -- Calculated when round closes
                                        }

                                    updatedRound =
                                        { round | guesses = Dict.insert user.telegramUser.id guess round.guesses }

                                    newModel =
                                        { model | rounds = Dict.insert roundId updatedRound model.rounds }
                                in
                                ( newModel
                                , broadcastGuessSubmitted user.telegramUser.id location model
                                )
                        else
                            ( model
                            , sendToFrontend clientId (ErrorMessage "This round is closed")
                            )

                    Nothing ->
                        ( model
                        , sendToFrontend clientId (ErrorMessage "Round not found")
                        )

        (Nothing, _) ->
            ( model
            , sendToFrontend clientId (ErrorMessage "Authentication required")
            )

        (_, Nothing) ->
            ( model
            , sendToFrontend clientId (ErrorMessage "No active round")
            )


handleEndRound : SessionId -> ClientId -> Model -> ( Model, Cmd BackendMsg )
handleEndRound sessionId clientId model =
    case (getUserFromSession sessionId model, model.currentRoundId) of
        (Just user, Just roundId) ->
            if user.isRay then
                case Dict.get roundId model.rounds of
                    Just round ->
                        let
                            -- Calculate distances for all guesses
                            updatedGuesses =
                                round.guesses
                                    |> Dict.map (\_ guess ->
                                        { guess 
                                        | distanceKm = Just (calculateDistance guess.location round.actualLocation)
                                        }
                                    )

                            closedRound =
                                { round
                                | isOpen = False
                                , endTime = Just (Time.millisToPosix 0)  -- Will be set properly
                                , guesses = updatedGuesses
                                }

                            newModel =
                                { model
                                | rounds = Dict.insert roundId closedRound model.rounds
                                , currentRoundId = Nothing
                                }
                        in
                        ( newModel
                        , broadcastRoundClosed closedRound model
                        )

                    Nothing ->
                        ( model
                        , sendToFrontend clientId (ErrorMessage "Round not found")
                        )
            else
                ( model
                , sendToFrontend clientId (ErrorMessage "Only Ray can end rounds")
                )

        (Nothing, _) ->
            ( model
            , sendToFrontend clientId (ErrorMessage "Authentication required")
            )

        (_, Nothing) ->
            ( model
            , sendToFrontend clientId (ErrorMessage "No active round")
            )


-- GAME STATE

handleRequestGameState : SessionId -> ClientId -> Model -> ( Model, Cmd BackendMsg )
handleRequestGameState sessionId clientId model =
    let
        currentUser = getUserFromSession sessionId model
        
        _ = Debug.log "🔍 RequestGameState" 
            { sessionId = sessionId
            , currentUser = Maybe.map (.telegramUser >> .firstName) currentUser
            , userSessions = Dict.keys model.userSessions
            }
        
        currentRound =
            case model.currentRoundId of
                Just roundId ->
                    Dict.get roundId model.rounds
                Nothing ->
                    Nothing

        pastRounds =
            model.rounds
                |> Dict.values
                |> List.filter (not << .isOpen)
                |> List.sortBy (.startTime >> Time.posixToMillis >> negate)  -- Most recent first

        gameState =
            { currentUser = currentUser
            , currentRound = currentRound
            , pastRounds = pastRounds
            }
    in
    ( model
    , sendToFrontend clientId (GameStateUpdate gameState)
    )


handleRequestRoundHistory : SessionId -> ClientId -> Model -> ( Model, Cmd BackendMsg )
handleRequestRoundHistory sessionId clientId model =
    let
        pastRounds =
            model.rounds
                |> Dict.values
                |> List.filter (not << .isOpen)
                |> List.sortBy (.startTime >> Time.posixToMillis >> negate)

        currentUser = getUserFromSession sessionId model

        gameState =
            { currentUser = currentUser
            , currentRound = Nothing
            , pastRounds = pastRounds
            }
    in
    ( model
    , sendToFrontend clientId (GameStateUpdate gameState)
    )


-- BROADCASTING

broadcastRoundCreated : Round -> Model -> Cmd BackendMsg
broadcastRoundCreated round model =
    let
        allClientIds =
            model.sessionClients
                |> Dict.values
                |> List.concat

        send clientId =
            sendToFrontend clientId (RoundCreated round)
    in
    Cmd.batch (List.map send allClientIds)


broadcastGuessSubmitted : Int -> Location -> Model -> Cmd BackendMsg
broadcastGuessSubmitted userId location model =
    let
        allClientIds =
            model.sessionClients
                |> Dict.values
                |> List.concat

        send clientId =
            sendToFrontend clientId (GuessSubmitted userId location)
    in
    Cmd.batch (List.map send allClientIds)


broadcastRoundClosed : Round -> Model -> Cmd BackendMsg
broadcastRoundClosed round model =
    let
        allClientIds =
            model.sessionClients
                |> Dict.values
                |> List.concat

        send clientId =
            sendToFrontend clientId (RoundClosed round)
    in
    Cmd.batch (List.map send allClientIds)


-- HELPER FUNCTIONS

getUserFromSession : SessionId -> Model -> Maybe User
getUserFromSession sessionId model =
    model.userSessions
        |> Dict.get sessionId
        |> Maybe.andThen (\userId -> Dict.get userId model.users)


generateRoundId : () -> String
generateRoundId _ =
    -- Simple ID generation - in production, use proper UUID generation
    "round_" ++ String.fromInt (Time.posixToMillis (Time.millisToPosix 0))


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ onConnect Connected
        , onDisconnect Disconnected
        ]


-- CONFIGURATION

-- Ray's Telegram Configuration
-- You'll need to update this with Ray's actual Telegram ID
rayConfig : { telegramId : Int }
rayConfig =
    { telegramId = 123456789  -- Replace with Ray's actual Telegram ID
    }