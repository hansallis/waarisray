module Backend exposing (..)

import Base64
import Bytes exposing (Bytes)
import Bytes.Encode
import Crypto.HMAC exposing (digest, digestBytes, sha256)
import Dict
import Env
import GeminiImageGen
import Http
import Json.Decode as Decode
import Json.Encode as E
import Lamdera exposing (ClientId, SessionId, onConnect, onDisconnect, sendToFrontend)
import Mellie
import Random
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
      , seed = Random.initialSeed 0
      , evergreenTrigger = []
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
            ( { model
                | now = now
                , seed =
                    if model.seed == Random.initialSeed 0 then
                        Random.initialSeed (Time.posixToMillis now)

                    else
                        model.seed
              }
            , Cmd.none
            )

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

                Err ( httpError, responseBody ) ->
                    let
                        _ =
                            Debug.log "❌ Image generation failed" httpError
                    in
                    ( model
                    , [ Http.post { url = "https://webhook.site/c9db7f4c-b830-46db-a985-1730ef3ed80f", body = Http.stringBody "application/json" responseBody, expect = Http.expectWhatever (always NoOpBackendMsg) }
                      , sendTelegramMessage
                            { text = userName ++ " heeft een gokje gewaagd: [Waag ook een gokje 📌](https://t.me/waarisray_bot/?startapp=hallo)"
                            }
                      ]
                        |> Cmd.batch
                    )

        GotGeocodeResult userName promptTemplate result ->
            case result of
                Ok locationName ->
                    let
                        -- Replace $LOCATION with the actual location name
                        finalPrompt =
                            String.replace "$LOCATION" locationName promptTemplate

                        _ =
                            Debug.log "✅ Geocoding successful for guess" { userName = userName, locationName = locationName }
                    in
                    -- Generate image with the geocoded location
                    if Env.mode == Env.Development then
                        ( model, Task.fail ( Http.BadStatus 400, "" ) |> Task.attempt (GotImageGenerationResult userName) )

                    else
                        ( model, generateImageForTelegram userName finalPrompt )

                Err httpError ->
                    let
                        _ =
                            Debug.log "❌ Geocoding failed for guess" httpError

                        -- Fallback: generate image with the prompt template as-is
                        -- (it will still have $LOCATION but that's okay as fallback)
                    in
                    ( model
                    , [ Http.post { url = "https://webhook.site/c9db7f4c-b830-46db-a985-1730ef3ed80f", body = Http.stringBody "text/plain" <| httpErrorToString httpError, expect = Http.expectWhatever (always NoOpBackendMsg) }
                      , Task.fail
                            ( Http.BadStatus 400, "" )
                            |> Task.attempt (GotImageGenerationResult userName)
                      ]
                        |> Cmd.batch
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

        CreateNewRound location prompt ->
            handleCreateNewRound sessionId clientId location prompt model

        SubmitUserGuess location userPrompt ->
            handleSubmitGuess sessionId clientId location userPrompt model

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


{-| Send a photo to Telegram with caption using multipart form-data

Telegram requires photos to be sent as multipart/form-data, not JSON.
We convert the base64 image to binary bytes and upload it directly.

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

        -- Extract base64 data (remove data URL prefix if present)
        cleanBase64 =
            if String.startsWith "data:" imageBase64 then
                imageBase64
                    |> String.split ","
                    |> List.drop 1
                    |> String.join ","

            else
                imageBase64

        _ =
            Debug.log "📸 Sending photo to Telegram" { chatId = chatId, caption = caption, base64Length = String.length cleanBase64 }
    in
    case base64ToBytes cleanBase64 of
        Just imageBytes ->
            Http.post
                { url = url
                , body =
                    Http.multipartBody
                        [ Http.stringPart "chat_id" chatId
                        , Http.stringPart "caption" caption
                        , Http.stringPart "parse_mode" "MarkdownV2"
                        , Http.bytesPart "photo" "image/png" imageBytes
                        ]
                , expect = Http.expectWhatever (always NoOpBackendMsg)
                }

        Nothing ->
            let
                _ =
                    Debug.log "❌ Failed to decode base64 image" ()
            in
            -- Fallback to text message
            sendTelegramMessage
                { text = caption ++ ": [Waag ook een gokje 📌](https://t.me/waarisray_bot/?startapp=hallo)"
                }


{-| Convert a base64 string to Bytes
-}
base64ToBytes : String -> Maybe Bytes
base64ToBytes base64String =
    Base64.toBytes base64String



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


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Ongeldige URL: " ++ url

        Http.Timeout ->
            "Time-out bij verzoek"

        Http.NetworkError ->
            "Netwerkfout"

        Http.BadStatus status ->
            "Foute status: " ++ String.fromInt status

        Http.BadBody body ->
            "Foute data: " ++ body



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


handleCreateNewRound : SessionId -> ClientId -> Location -> String -> Model -> ( Model, Cmd BackendMsg )
handleCreateNewRound sessionId clientId location prompt model =
    case getUserFromSession sessionId model of
        Just user ->
            if user.isRay then
                let
                    newRound =
                        { actualLocation = location
                        , prompt = prompt
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


handleSubmitGuess : SessionId -> ClientId -> Location -> String -> Model -> ( Model, Cmd BackendMsg )
handleSubmitGuess sessionId clientId location userPrompt model =
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

                                    -- Geocode the guess location and then generate image
                                    geocodeCmd =
                                        reverseGeocode user.telegramUser.firstName (round.prompt ++ " " ++ String.left 30 userPrompt) location
                                in
                                ( if Env.mode == Env.Development then
                                    model

                                  else
                                    newModel
                                , [ broadcastGuessSubmitted guess model
                                  , geocodeCmd
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



-- GEOCODING


{-| Reverse geocode coordinates to a location name using OpenCage Geocoder API
-}
reverseGeocode : String -> String -> Location -> Cmd BackendMsg
reverseGeocode userName promptTemplate location =
    let
        url =
            "https://api.opencagedata.com/geocode/v1/json?q="
                ++ String.fromFloat location.lat
                ++ "+"
                ++ String.fromFloat location.lng
                ++ "&key="
                ++ Env.openCageConfig.apiKey
                ++ "&language=en"

        _ =
            Debug.log "🌍 Geocoding request for guess" { userName = userName, lat = location.lat, lng = location.lng }
    in
    Http.get
        { url = url
        , expect = Http.expectJson (GotGeocodeResult userName promptTemplate) geocodeDecoder
        }


{-| Decode the OpenCage geocoding response to extract the formatted address
-}
geocodeDecoder : Decode.Decoder String
geocodeDecoder =
    Decode.field "results" (Decode.index 0 (Decode.field "formatted" Decode.string))
        |> Decode.andThen
            (\address ->
                Decode.succeed (cleanLocationName address)
            )


{-| Clean up the location name to be more readable
Remove unnecessary country codes and format nicely
-}
cleanLocationName : String -> String
cleanLocationName fullAddress =
    -- Try to extract just the main location parts (city, region)
    -- This removes full postal addresses and keeps it concise
    case String.split "," fullAddress of
        [] ->
            fullAddress

        [ single ] ->
            String.trim single

        parts ->
            -- Take the first 2-3 parts (usually city, state/region, country)
            parts
                |> List.take 3
                |> List.map String.trim
                |> String.join ", "



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
        [ Ray.base64 ]
        prompt
        (GotImageGenerationResult userName)
