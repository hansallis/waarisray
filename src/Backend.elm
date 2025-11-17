module Backend exposing (..)

import Base64.Encode
import Crypto.HMAC exposing (digest, digestBytes, sha256)
import Dict
import Env
import GeminiImageGen
import Http
import Json.Decode as Decode
import Json.Encode as E
import Lamdera exposing (ClientId, SessionId, onConnect, onDisconnect, sendToFrontend)
import Ray
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
      , rounds = []
      , userSessions = Dict.empty
      , failedAuthentications = []
      , now = Time.millisToPosix 0
      }
    , Cmd.none
    )



-- UPDATE


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        Tick now ->
            ( { model | now = now }, Cmd.none )

        GotImageGenerationResult userName result ->
            -- Handle image generation result and send to Telegram
            case result of
                Ok generationResult ->
                    let
                        -- Get the first generated image
                        cmd =
                            case List.head generationResult.images of
                                Just imageBase64 ->
                                    sendTelegramPhoto
                                        { caption = userName ++ " heeft een gokje gewaagd: [Waag ook een gokje 📌](https://t.me/waarisray_bot/?startapp=hallo)"
                                        , imageBase64 = imageBase64
                                        }

                                Nothing ->
                                    -- Fallback to text message if no image
                                    sendTelegramMessage
                                        { text = userName ++ " heeft een gokje gewaagd: [Waag ook een gokje 📌](https://t.me/waarisray_bot/?startapp=hallo)"
                                        }
                    in
                    ( model, cmd )

                Err httpError ->
                    let
                        _ =
                            Debug.log "❌ Image generation failed" httpError
                    in
                    ( model
                    , sendTelegramMessage
                        { text = userName ++ " heeft een gokje gewaagd: [Waag ook een gokje 📌](https://t.me/waarisray_bot/?startapp=hallo)"
                        }
                    )


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
            , sendToFrontend clientId (AuthenticationResult (Err error))
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


{-| Send a photo to Telegram with caption

Note: Telegram's Bot API requires the photo to be sent as a URL or file.
Since we have a base64 image from Gemini, we need to use a data URL approach.
For production, you might want to upload to a CDN first and send the URL instead.

For now, we'll use the simpler approach of sending the base64 as-is and let
Telegram handle it (this works with the photo field accepting base64 data URLs).

If this doesn't work, an alternative is to:

1.  Upload the image to a temporary storage/CDN
2.  Send the URL to Telegram
3.  Clean up after

-}
sendTelegramPhoto :
    { caption : String
    , imageBase64 : String
    }
    -> Cmd BackendMsg
sendTelegramPhoto { caption, imageBase64 } =
    let
        chatId =
            if Env.mode == Env.Development then
                "8814284"

            else
                Env.telegramConfig.code15id

        url =
            "https://api.telegram.org/bot"
                ++ Env.telegramConfig.botToken
                ++ "/sendPhoto"

        -- For Telegram, we can send the image as a data URL directly
        -- The imageBase64 from Gemini already includes the data:image/png;base64, prefix
        body =
            E.object
                [ ( "chat_id", E.string chatId )
                , ( "photo", E.string imageBase64 )
                , ( "caption", E.string caption )
                , ( "parse_mode", E.string "MarkdownV2" )
                ]
                |> Http.jsonBody

        _ =
            Debug.log "📸 Sending photo to Telegram" { chatId = chatId, captionLength = String.length caption, imageLength = String.length imageBase64 }
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
                    newRound =
                        { actualLocation = location
                        , startTime = model.now -- Will be set properly with Time.now
                        , endTime = Nothing
                        , guesses = Dict.empty
                        , isOpen = True
                        }

                    newModel =
                        { model
                            | rounds = newRound :: model.rounds
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
    case getUserFromSession sessionId model of
        Just user ->
            if user.isRay then
                ( model
                , sendToFrontend clientId (ErrorMessage "Ray cannot submit guesses")
                )

            else
                case model.rounds of
                    round :: oldRounds ->
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
                                        , timestamp = model.now -- Will be set properly
                                        , distanceKm = Nothing -- Calculated when round closes
                                        }

                                    updatedRound =
                                        { round | guesses = Dict.insert user.telegramUser.id guess round.guesses }

                                    newModel =
                                        { model | rounds = updatedRound :: oldRounds }

                                    -- Generate image with location in prompt
                                    imagePrompt =
                                        --if String.contains "Melvin" user.telegramUser.firstName then
                                        --    "Create a realistic photo of the attached person who travelled as a pilot to the following coordinates: "
                                        --        ++ String.fromFloat location.lat
                                        --        ++ ", "
                                        --        ++ String.fromFloat location.lng
                                        --        ++ ". Make it funny, and engaging with landmarks or geographical features if recognizable. Make him recognizable as an airline pilot working for KLM. "
                                        --        ++ "If the location is clearly in the middle of a sea or the ocean, create something funny and possibly disturbing. He might've crashed his plane or parachuted on a boat. "
                                        --        ++ "If the location's in The Netherlands, try and use something specific from the city/town he's in. "
                                        --
                                        --else
                                        "Create a realistic photo of the attached person who travelled as a pilot to the following coordinates: "
                                            ++ String.fromFloat location.lat
                                            ++ ", "
                                            ++ String.fromFloat location.lng
                                            ++ ". Make it funny, and engaging with landmarks or geographical features if recognizable. Make him recognizable as an airline pilot working for KLM. "
                                            ++ "If the location is clearly in the middle of a sea or the ocean, create something funny and possibly disturbing. He might've crashed his plane or parachuted on a boat. "
                                            ++ "If the location's in The Netherlands, try and use something specific from the city/town he's in. "

                                    imageGenCmd =
                                        generateImageForTelegram user.telegramUser.firstName imagePrompt
                                in
                                ( model
                                , [ broadcastGuessSubmitted guess model
                                  , imageGenCmd
                                  ]
                                    |> Cmd.batch
                                )

                        else
                            ( model
                            , sendToFrontend clientId (ErrorMessage "This round is closed")
                            )

                    [] ->
                        ( model
                        , sendToFrontend clientId (ErrorMessage "No active round")
                        )

        Nothing ->
            ( model
            , sendToFrontend clientId (ErrorMessage "Authentication required")
            )


handleEndRound : SessionId -> ClientId -> Model -> ( Model, Cmd BackendMsg )
handleEndRound sessionId clientId model =
    case getUserFromSession sessionId model of
        Just user ->
            if user.isRay then
                case model.rounds of
                    round :: tail ->
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
                                    , endTime = Just model.now -- Will be set properly
                                    , guesses = updatedGuesses
                                }

                            newModel =
                                { model
                                    | rounds = closedRound :: tail
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

                    [] ->
                        ( model
                        , sendToFrontend clientId (ErrorMessage "Round not found")
                        )

            else
                ( model
                , sendToFrontend clientId (ErrorMessage "Only Ray can end rounds")
                )

        Nothing ->
            ( model
            , sendToFrontend clientId (ErrorMessage "Authentication required")
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
            List.head model.rounds

        pastRounds =
            model.rounds
                |> List.tail
                |> Maybe.withDefault []
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
    Time.every 1000 Tick


{-| Generate an image for Telegram using Gemini AI
-}
generateImageForTelegram : String -> String -> Cmd BackendMsg
generateImageForTelegram userName prompt =
    GeminiImageGen.generateFromImageAndText
        Env.geminiConfig
        Ray.base64
        prompt
        (GotImageGenerationResult userName)
