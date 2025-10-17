port module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Env
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Lamdera
import Time
import Types exposing (..)
import Url



-- PORTS


port authenticateWithTelegram : () -> Cmd msg


port telegramAuthResult : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , currentUser = Nothing
      , page = LoginPage
      , currentRound = Nothing
      , pastRounds = []
      , userGuess = Nothing
      , pendingLocation = Nothing
      , showingGuesses = False
      , mapCenter = initialLocation
      , mapZoom = 2
      , error = Nothing
      , authData = Nothing
      }
    , Cmd.batch
        [ Lamdera.sendToBackend RequestCurrentGameState
        ]
    )



-- UPDATE


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            ( model, Cmd.none )

        InitiateTelegramAuth ->
            let
                _ =
                    Debug.log "🔵 InitiateTelegramAuth clicked!" ()
            in
            ( model, authenticateWithTelegram () )

        TelegramAuthResult (Ok authData) ->
            let
                _ =
                    Debug.log "🟢 TelegramAuthResult received:" authData
            in
            ( { model | authData = Just authData }
            , Lamdera.sendToBackend (AuthenticateWithTelegram authData)
            )

        TelegramAuthResult (Err _) ->
            ( { model | error = Just "Failed to authenticate with Telegram" }
            , Cmd.none
            )

        Logout ->
            let
                _ =
                    Debug.log "🚪 Logout clicked" ()
            in
            ( { model | currentUser = Nothing, page = LoginPage, error = Nothing }
            , Lamdera.sendToBackend LogoutUser
            )

        LoginAsRay ->
            let
                _ =
                    Debug.log "👑 Login as Ray clicked" ()
            in
            ( model
            , Lamdera.sendToBackend AuthenticateAsRay
            )

        LoginAsRegularUser ->
            let
                _ =
                    Debug.log "👤 Login as Regular User clicked" ()
            in
            ( model
            , Lamdera.sendToBackend AuthenticateAsRegularUser
            )

        MapClicked location ->
            case model.currentUser of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    if user.isRay && model.page == GamePage then
                        -- Ray is setting the location for a new round
                        case model.currentRound of
                            Nothing ->
                                -- No active round - set pending location
                                ( { model | pendingLocation = Just location }
                                , Cmd.none
                                )

                            Just _ ->
                                -- Active round exists - ignore click
                                ( model, Cmd.none )

                    else if not user.isRay && model.currentRound /= Nothing then
                        -- Regular user making a guess (only if there's an active round)
                        -- Check if they've already submitted their guess
                        case model.currentRound of
                            Just round ->
                                if Dict.member user.telegramUser.id round.guesses then
                                    -- Guess already submitted, can't change it
                                    ( model, Cmd.none )

                                else
                                    -- Can still adjust guess before submitting
                                    ( { model | userGuess = Just location }
                                    , Cmd.none
                                    )

                            Nothing ->
                                ( model, Cmd.none )

                    else
                        ( model, Cmd.none )

        SetMapCenter location zoom ->
            ( { model | mapCenter = location, mapZoom = round zoom }
            , Cmd.none
            )

        StartNewRound ->
            case model.currentUser of
                Just user ->
                    if user.isRay then
                        -- Clear the current round and reset UI state so Ray can start a new round
                        ( { model
                            | currentRound = Nothing
                            , page = GamePage
                            , userGuess = Nothing
                            , showingGuesses = False
                            , pendingLocation = Nothing
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ConfirmStartRound ->
            case ( model.currentUser, model.pendingLocation ) of
                ( Just user, Just location ) ->
                    if user.isRay then
                        ( { model | pendingLocation = Nothing }
                        , Lamdera.sendToBackend (CreateNewRound location)
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CancelPendingLocation ->
            ( { model | pendingLocation = Nothing }
            , Cmd.none
            )

        SubmitGuess ->
            case ( model.currentUser, model.userGuess ) of
                ( Just user, Just guess ) ->
                    if not user.isRay then
                        ( { model | userGuess = Nothing }
                          -- Clear local guess, will be set from broadcast
                        , Lamdera.sendToBackend (SubmitUserGuess guess)
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CloseRound ->
            case model.currentUser of
                Just user ->
                    if user.isRay then
                        ( model
                        , Lamdera.sendToBackend EndCurrentRound
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ViewRound roundId ->
            ( { model | page = ResultsPage roundId }
            , Cmd.none
            )

        GoToHistory ->
            ( { model | page = HistoryPage }
            , Lamdera.sendToBackend RequestRoundHistory
            )

        GoToGame ->
            ( { model | page = GamePage }
            , Lamdera.sendToBackend RequestCurrentGameState
            )

        ToggleGuessesVisibility ->
            ( { model | showingGuesses = not model.showingGuesses }
            , showAllGuesses model
            )

        ClearError ->
            ( { model | error = Nothing }
            , Cmd.none
            )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        AuthenticationResult (Ok user) ->
            ( { model | currentUser = Just user, page = GamePage, error = Nothing }
            , Lamdera.sendToBackend RequestCurrentGameState
            )

        AuthenticationResult (Err error) ->
            ( { model | error = Just error }
            , Cmd.none
            )

        GameStateUpdate { currentUser, currentRound, pastRounds } ->
            let
                _ =
                    Debug.log "🔄 GameStateUpdate received"
                        { currentUser = Maybe.map (.telegramUser >> .firstName) currentUser
                        , currentPage = model.page
                        , hasCurrentRound = currentRound /= Nothing
                        }

                -- If we receive a currentUser and we're on login page, go to game page
                newPage =
                    case ( currentUser, model.page ) of
                        ( Just _, LoginPage ) ->
                            GamePage

                        _ ->
                            model.page

                newModel =
                    { model
                        | currentUser = currentUser
                        , currentRound = currentRound
                        , pastRounds = pastRounds
                        , page = newPage
                    }
            in
            ( newModel
            , case currentRound of
                Just round ->
                    Cmd.none

                Nothing ->
                    Cmd.none
            )

        RoundCreated round ->
            ( { model | currentRound = Just round }
            , Cmd.none
            )

        GuessSubmitted guess ->
            case model.currentUser of
                Just user ->
                    let
                        -- Update the current round with the new guess
                        updatedModel =
                            { model
                                | currentRound =
                                    case model.currentRound of
                                        Just round ->
                                            Just { round | guesses = Dict.insert guess.userId guess round.guesses }

                                        Nothing ->
                                            Nothing
                            }

                        -- If this is the current user's own guess, also update userGuess
                        finalModel =
                            if guess.userId == user.telegramUser.id then
                                { updatedModel | userGuess = Just guess.location }

                            else
                                updatedModel
                    in
                    ( finalModel, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        RoundClosed round ->
            ( { model | currentRound = Just round, pastRounds = round :: model.pastRounds }
            , Cmd.none
            )

        ErrorMessage error ->
            ( { model | error = Just error }
            , Cmd.none
            )



-- HELPER FUNCTIONS


showAllGuesses : Model -> Cmd FrontendMsg
showAllGuesses model =
    {- case model.currentRound of
       Just round ->
           let
               guessMarkers =
                   round.guesses
                       |> Dict.values
                       |> List.map
                           (\guess ->
                               addMarker
                                   { location = guess.location
                                   , color = "red"
                                   , label = "Guess"
                                   }
                           )
           in
           Cmd.batch guessMarkers

       Nothing ->
    -}
    Cmd.none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ telegramAuthResult (\result -> TelegramAuthResult (Ok result))
        ]



-- VIEW


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Where is Ray?"
    , body =
        [ div [ class "app" ]
            [ viewHeader model
            , viewError model
            , viewContent model
            ]
        , viewStyles
        ]
    }


viewHeader : Model -> Html FrontendMsg
viewHeader model =
    header [ class "header" ]
        [ h1 [] [ text "Where is Ray? ✈️" ]
        , nav [ class "nav" ]
            [ case model.currentUser of
                Just user ->
                    div [ class "user-info" ]
                        [ span [] [ text ("Welcome, " ++ user.telegramUser.firstName) ]
                        , if user.isRay then
                            span [ class "ray-badge" ] [ text "Pilot" ]

                          else
                            text ""
                        , button [ class "logout-btn", onClick Logout ] [ text "Logout" ]
                        ]

                Nothing ->
                    text ""
            , case model.currentUser of
                Just _ ->
                    div [ class "nav-buttons" ]
                        [ button
                            [ onClick GoToGame
                            , classList [ ( "active", model.page == GamePage ) ]
                            ]
                            [ text "Game" ]
                        , button
                            [ onClick GoToHistory
                            , classList [ ( "active", model.page == HistoryPage ) ]
                            ]
                            [ text "History" ]
                        ]

                Nothing ->
                    text ""
            ]
        ]


viewError : Model -> Html FrontendMsg
viewError model =
    case model.error of
        Just error ->
            div [ class "error" ]
                [ text error
                , button [ onClick ClearError ] [ text "×" ]
                ]

        Nothing ->
            text ""


viewContent : Model -> Html FrontendMsg
viewContent model =
    case model.page of
        LoginPage ->
            viewLoginPage

        GamePage ->
            viewGamePage model

        ResultsPage roundId ->
            viewResultsPage model roundId

        HistoryPage ->
            viewHistoryPage model


viewLoginPage : Html FrontendMsg
viewLoginPage =
    div [ class "login-page" ]
        [ div [ class "login-container" ]
            ([ h2 [] [ text "Login with Telegram" ]
             , p [] [ text "Authenticate using Telegram to start guessing Ray's location!" ]
             , button
                [ class "telegram-login-btn"
                , onClick InitiateTelegramAuth
                ]
                [ text "Login with Telegram" ]
             ]
                ++ (if Env.mode == Env.Production then
                        []

                    else
                        [ div [ class "test-login-section" ]
                            [ h3 [] [ text "Development Testing" ]
                            , p [] [ text "Quick login buttons for testing:" ]
                            , button
                                [ class "test-login-btn ray-btn"
                                , onClick LoginAsRay
                                ]
                                [ text "Login as Ray (Pilot) ✈️" ]
                            , button
                                [ class "test-login-btn user-btn"
                                , onClick LoginAsRegularUser
                                ]
                                [ text "Login as Regular User 👤" ]
                            ]
                        ]
                   )
            )
        ]


viewGamePage : Model -> Html FrontendMsg
viewGamePage model =
    case model.currentUser of
        Nothing ->
            viewLoginPage

        Just user ->
            div [ class "game-page" ]
                [ viewGameControls model user
                , viewMap model
                , viewGameStatus model user
                ]


viewGameControls : Model -> User -> Html FrontendMsg
viewGameControls model user =
    div [ class "game-controls" ]
        [ if user.isRay then
            viewRayControls model

          else
            viewPlayerControls model
        ]


viewRayControls : Model -> Html FrontendMsg
viewRayControls model =
    case model.currentRound of
        Nothing ->
            case model.pendingLocation of
                Just location ->
                    div [ class "pending-location" ]
                        [ h3 [] [ text "Confirm Location" ]
                        , p [] [ text ("Latitude: " ++ String.fromFloat location.lat) ]
                        , p [] [ text ("Longitude: " ++ String.fromFloat location.lng) ]
                        , div [ class "button-group" ]
                            [ button [ class "confirm-btn", onClick ConfirmStartRound ] [ text "✓ Start Round" ]
                            , button [ class "cancel-btn", onClick CancelPendingLocation ] [ text "✗ Cancel" ]
                            ]
                        ]

                Nothing ->
                    div []
                        [ h3 [] [ text "Start a new round" ]
                        , p [] [ text "Click on the map to set your current location" ]
                        ]

        Just round ->
            if round.isOpen then
                div []
                    [ h3 [] [ text "Round in progress" ]
                    , p [] [ text ("Started: " ++ formatTime round.startTime) ]
                    , p [] [ text ("Guesses: " ++ String.fromInt (Dict.size round.guesses)) ]
                    , h4 [] [ text "Current Guesses" ]
                    , viewCurrentGuessesTable round
                    , button [ onClick CloseRound ] [ text "Close Round" ]
                    ]

            else
                div []
                    [ h3 [] [ text "Round completed" ]
                    , viewRoundResults round
                    , button [ onClick StartNewRound ] [ text "Start New Round" ]
                    ]


viewCurrentGuessesTable : Round -> Html FrontendMsg
viewCurrentGuessesTable round =
    let
        guessesWithDistance =
            round.guesses
                |> Dict.values
                |> List.map
                    (\guess ->
                        let
                            distance =
                                calculateDistance guess.location round.actualLocation
                        in
                        { guess = guess
                        , distance = distance
                        }
                    )
                |> List.sortBy .distance
    in
    if List.isEmpty guessesWithDistance then
        div [ class "current-guesses" ]
            [ p [ class "no-guesses" ] [ text "No guesses yet" ] ]

    else
        div [ class "current-guesses" ]
            [ h4 [] [ text "Current Guesses" ]
            , div [ class "guesses-list" ]
                (guessesWithDistance
                    |> List.indexedMap
                        (\index { guess, distance } ->
                            div [ class "guess-item" ]
                                [ span [ class "rank" ] [ text (String.fromInt (index + 1) ++ ".") ]
                                , span [ class "user" ] [ text guess.userName ]
                                , span [ class "distance" ]
                                    [ text (String.fromInt (Basics.round distance) ++ " km") ]
                                ]
                        )
                )
            ]


viewPlayerControls : Model -> Html FrontendMsg
viewPlayerControls model =
    case model.currentRound of
        Nothing ->
            -- Show most recent round results
            case List.head model.pastRounds of
                Just mostRecentRound ->
                    div []
                        [ h3 [] [ text "Most Recent Round" ]
                        , p [] [ text "Waiting for Ray to start a new round..." ]
                        , viewRoundResults mostRecentRound
                        ]

                Nothing ->
                    div []
                        [ h3 [] [ text "No active round" ]
                        , p [] [ text "Waiting for Ray to start the first round..." ]
                        ]

        Just round ->
            if round.isOpen then
                case model.userGuess of
                    Nothing ->
                        div []
                            [ h3 [] [ text "Make your guess!" ]
                            , p [] [ text "Click on the map where you think Ray is located" ]
                            ]

                    Just _ ->
                        div []
                            [ h3 [] [ text "Guess submitted!" ]
                            , button [ onClick SubmitGuess ] [ text "Confirm Guess" ]
                            , p [] [ text ("Other players have made " ++ String.fromInt (Dict.size round.guesses) ++ " guesses") ]
                            , button [ onClick ToggleGuessesVisibility ]
                                [ text
                                    (if model.showingGuesses then
                                        "Hide Other Guesses"

                                     else
                                        "Show Other Guesses"
                                    )
                                ]
                            ]

            else
                div []
                    [ h3 [] [ text "Round completed!" ]
                    , viewRoundResults round
                    ]


viewGameStatus : Model -> User -> Html FrontendMsg
viewGameStatus model user =
    case model.currentRound of
        Just round ->
            div [ class "game-status" ]
                [ h4 [] [ text "Round Status" ]
                , p [] [ text "Created by: Ray" ]
                , p [] [ text ("Started: " ++ formatTime round.startTime) ]
                , case round.endTime of
                    Just endTime ->
                        p [] [ text ("Ended: " ++ formatTime endTime) ]

                    Nothing ->
                        p [] [ text "Status: Open" ]
                , p [] [ text ("Total guesses: " ++ String.fromInt (Dict.size round.guesses)) ]
                ]

        Nothing ->
            text ""


viewMap : Model -> Html FrontendMsg
viewMap model =
    let
        markers =
            case model.currentRound of
                Just round ->
                    if round.isOpen then
                        -- Active open round
                        case model.currentUser of
                            Just user ->
                                if user.isRay then
                                    -- Ray sees actual location and all guesses
                                    [ viewMapMarker "piloticon" round.actualLocation [ b [] [ text "Your Location" ], p [] [ text "Where you are" ] ] ]
                                        ++ (round.guesses
                                                |> Dict.values
                                                |> List.map
                                                    (\guess ->
                                                        viewMapMarker "questionicon" guess.location [ b [] [ text guess.userName ], p [] [ text "Guess" ] ]
                                                    )
                                           )

                                else
                                    -- Regular users see their own pending guess (if any)
                                    case model.userGuess of
                                        Just guessLocation ->
                                            [ viewMapMarker "questionicon" guessLocation [ b [] [ text "Your Guess" ], p [] [ text "Click confirm to submit" ] ] ]

                                        Nothing ->
                                            []

                            Nothing ->
                                []

                    else
                        -- Closed round - show everything
                        [ viewMapMarker "piloticon" round.actualLocation [ b [] [ text "Actual location" ], p [] [ text "Ray was here" ] ] ]
                            ++ (round.guesses
                                    |> Dict.values
                                    |> List.map
                                        (\guess ->
                                            viewMapMarker "questionicon" guess.location [ b [] [ text guess.userName ], p [] [ text "Guess" ] ]
                                        )
                               )

                Nothing ->
                    -- No active round
                    case model.currentUser of
                        Just user ->
                            if user.isRay then
                                -- Ray sees pending location
                                case model.pendingLocation of
                                    Just location ->
                                        [ viewMapMarker "piloticon" location [ b [] [ text "Pending Location" ], p [] [ text "Click confirm to start round" ] ] ]

                                    Nothing ->
                                        []

                            else
                                -- Regular users see most recent round
                                case List.head model.pastRounds of
                                    Just mostRecentRound ->
                                        [ viewMapMarker "piloticon" mostRecentRound.actualLocation [ b [] [ text "Last Location" ], p [] [ text "Previous round" ] ] ]
                                            ++ (mostRecentRound.guesses
                                                    |> Dict.values
                                                    |> List.map
                                                        (\guess ->
                                                            viewMapMarker "questionicon" guess.location [ b [] [ text guess.userName ], p [] [ text "Guess" ] ]
                                                        )
                                               )

                                    Nothing ->
                                        []

                        Nothing ->
                            []
    in
    node "leaflet-map"
        [ on "click" clickDecoder, attribute "image-path" "/leaflet/x/", property "worldCopyJump" (Encode.bool True), attribute "zoom" "3", attribute "min-zoom" "3", attribute "max-zoom" "10" ]
        ([ node "leaflet-tilelayer" [ attribute "url" "https://api.maptiler.com/maps/topo-v2/{z}/{x}/{y}.png?key=DswcJkmNhAKYInVuYSU6" ] []
         , node "leaflet-scale-control" [ attribute "position" "bottomright", attribute "metric" "metric" ] []
         , node "leaflet-icon"
            [ attribute "icon-height" "46"
            , attribute "icon-width" "31"
            , attribute "icon-anchor-x" "15"
            , attribute "icon-anchor-y" "44"
            , attribute "icon-url" "https://api.geoapify.com/v2/icon/?type=awesome&color=red&size=42&icon=plane&contentSize=15&apiKey=535804d2778a49adb8bf12f7d55d3eea"
            , attribute "icon-retina-url" "https://api.geoapify.com/v2/icon/?type=awesome&color=red&size=42&icon=plane&contentSize=15&scaleFactor=2&apiKey=535804d2778a49adb8bf12f7d55d3eea"
            , id "piloticon"
            ]
            []
         , node "leaflet-icon"
            [ attribute "icon-height" "46"
            , attribute "icon-width" "31"
            , attribute "icon-anchor-x" "15"
            , attribute "icon-anchor-y" "44"
            , attribute "icon-url" "https://api.geoapify.com/v2/icon/?type=awesome&color=red&size=42&icon=question&contentSize=15&apiKey=535804d2778a49adb8bf12f7d55d3eea"
            , attribute "icon-retina-url" "https://api.geoapify.com/v2/icon/?type=awesome&color=red&size=42&icon=question&contentSize=15&scaleFactor=2&apiKey=535804d2778a49adb8bf12f7d55d3eea"
            , id "questionicon"
            ]
            []
         ]
            ++ markers
        )


viewMapMarker : String -> Location -> List (Html msg) -> Html msg
viewMapMarker icon location popup =
    node "leaflet-marker"
        [ attribute "icon" icon, attribute "latitude" (location.lat |> String.fromFloat), attribute "longitude" (location.lng |> String.fromFloat), attribute "title" "Actual location" ]
        popup


clickDecoder : Decode.Decoder FrontendMsg
clickDecoder =
    Decode.map2
        Location
        (Decode.at [ "detail", "latlng", "lat" ] Decode.float)
        (Decode.at [ "detail", "latlng", "lng" ] Decode.float)
        |> Decode.map MapClicked


viewResultsPage : Model -> String -> Html FrontendMsg
viewResultsPage model roundId =
    let
        round =
            model.pastRounds
                |> List.filter (\r -> r.id == roundId)
                |> List.head
    in
    case round of
        Just r ->
            div [ class "results-page" ]
                [ h2 [] [ text "Round Results" ]
                , viewRoundResults r
                , button [ onClick GoToGame ] [ text "Back to Game" ]
                ]

        Nothing ->
            div [ class "results-page" ]
                [ h2 [] [ text "Round not found" ]
                , button [ onClick GoToGame ] [ text "Back to Game" ]
                ]


viewHistoryPage : Model -> Html FrontendMsg
viewHistoryPage model =
    div [ class "history-page" ]
        [ h2 [] [ text "Previous Rounds" ]
        , if List.isEmpty model.pastRounds then
            p [] [ text "No previous rounds yet." ]

          else
            div [ class "rounds-list" ]
                (model.pastRounds
                    |> List.map viewRoundSummary
                )
        ]


viewRoundSummary : Round -> Html FrontendMsg
viewRoundSummary round =
    div [ class "round-summary" ]
        [ h4 [] [ text ("Round " ++ round.id) ]
        , p [] [ text ("Started: " ++ formatTime round.startTime) ]
        , case round.endTime of
            Just endTime ->
                p [] [ text ("Ended: " ++ formatTime endTime) ]

            Nothing ->
                text ""
        , p [] [ text ("Guesses: " ++ String.fromInt (Dict.size round.guesses)) ]
        , button [ onClick (ViewRound round.id) ] [ text "View Details" ]
        ]


viewRoundResults : Round -> Html FrontendMsg
viewRoundResults round =
    let
        sortedGuesses =
            round.guesses
                |> Dict.values
                |> List.sortBy (.distanceKm >> Maybe.withDefault 999999)
    in
    div [ class "round-results" ]
        [ h4 [] [ text "Results" ]
        , div [ class "results-list" ]
            (sortedGuesses
                |> List.indexedMap
                    (\index guess ->
                        div [ class "result-item" ]
                            [ span [ class "rank" ] [ text (String.fromInt (index + 1) ++ ".") ]
                            , span [ class "user" ] [ text guess.userName ]
                            , span [ class "distance" ]
                                [ text
                                    (case guess.distanceKm of
                                        Just dist ->
                                            String.fromInt (Basics.round dist) ++ " km"

                                        Nothing ->
                                            "Calculating..."
                                    )
                                ]
                            ]
                    )
            )
        ]



-- HELPER FUNCTIONS


formatTime : Time.Posix -> String
formatTime time =
    -- Simple time formatting - in a real app you'd use a proper time library
    String.fromInt (Time.posixToMillis time)



-- TELEGRAM AUTH
-- (Ports moved to top of file)
-- STYLES


viewStyles : Html msg
viewStyles =
    node "style"
        []
        [ text """
            * { margin: 0; padding: 0; box-sizing: border-box; }
            
            body { 
                font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
                background: #f5f7fa;
                color: #333;
            }
            
            .app { min-height: 100vh; display: flex; flex-direction: column; }
            
            .header {
                background: #2c3e50;
                color: white;
                padding: 1rem 2rem;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }
            
            .header h1 { margin-bottom: 1rem; }
            
            .nav { display: flex; justify-content: space-between; align-items: center; }
            
            .user-info { display: flex; align-items: center; gap: 1rem; }
            
            .ray-badge {
                background: #e74c3c;
                padding: 0.25rem 0.5rem;
                border-radius: 12px;
                font-size: 0.8rem;
                font-weight: bold;
            }
            
            .nav-buttons { display: flex; gap: 1rem; }
            
            .nav-buttons button {
                background: transparent;
                border: 2px solid white;
                color: white;
                padding: 0.5rem 1rem;
                border-radius: 4px;
                cursor: pointer;
                transition: all 0.2s;
            }
            
            .nav-buttons button:hover { background: white; color: #2c3e50; }
            .nav-buttons button.active { background: white; color: #2c3e50; }
            
            .error {
                background: #e74c3c;
                color: white;
                padding: 1rem 2rem;
                display: flex;
                justify-content: space-between;
                align-items: center;
            }
            
            .error button {
                background: transparent;
                border: none;
                color: white;
                font-size: 1.5rem;
                cursor: pointer;
            }
            
            .login-page {
                flex: 1;
                display: flex;
                align-items: center;
                justify-content: center;
            }
            
            .login-container {
                background: white;
                padding: 3rem;
                border-radius: 8px;
                box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                text-align: center;
                max-width: 400px;
            }
            
            .telegram-login-btn {
                background: #0088cc;
                color: white;
                border: none;
                padding: 1rem 2rem;
                border-radius: 6px;
                font-size: 1rem;
                cursor: pointer;
                margin-top: 2rem;
                transition: background 0.2s;
            }
            
            .telegram-login-btn:hover { background: #0077bb; }
            
            .game-page { flex: 1; display: flex; flex-direction: column; }
            
            .game-controls {
                background: white;
                padding: 1.5rem 2rem;
                border-bottom: 1px solid #ddd;
            }
            
            .map {
                flex: 1;
                min-height: 500px;
                background: #e8f4f8;
                position: relative;
            }
            
            .game-status {
                background: white;
                padding: 1.5rem 2rem;
                border-top: 1px solid #ddd;
            }
            
            .results-page, .history-page {
                flex: 1;
                padding: 2rem;
                max-width: 800px;
                margin: 0 auto;
            }
            
            .rounds-list { display: flex; flex-direction: column; gap: 1rem; }
            
            .round-summary {
                background: white;
                padding: 1.5rem;
                border-radius: 8px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }
            
            .round-results { margin-top: 2rem; }
            
            .results-list { display: flex; flex-direction: column; gap: 0.5rem; }
            
            .result-item {
                display: flex;
                gap: 1rem;
                padding: 0.75rem;
                background: #f8f9fa;
                border-radius: 4px;
                align-items: center;
            }
            
            .current-guesses {
                margin-top: 1.5rem;
                padding: 1rem;
                background: #f8f9fa;
                border-radius: 8px;
            }
            
            .current-guesses h4 {
                margin-bottom: 1rem;
                color: #2c3e50;
            }
            
            .no-guesses {
                color: #7f8c8d;
                font-style: italic;
            }
            
            .guesses-list { display: flex; flex-direction: column; gap: 0.5rem; }
            
            .guess-item {
                display: flex;
                gap: 1rem;
                padding: 0.75rem;
                background: white;
                border-radius: 4px;
                align-items: center;
                border-left: 3px solid #3498db;
            }
            
            .rank { font-weight: bold; min-width: 2rem; }
            .user { flex: 1; }
            .distance { font-weight: bold; color: #e74c3c; }
            
            button {
                background: #3498db;
                color: white;
                border: none;
                padding: 0.75rem 1.5rem;
                border-radius: 4px;
                cursor: pointer;
                font-size: 1rem;
                transition: background 0.2s;
                margin: 0.25rem;
            }
            
            button:hover { background: #2980b9; }
            
            button:disabled {
                background: #bdc3c7;
                cursor: not-allowed;
            }
            
                         h2, h3, h4 { margin-bottom: 1rem; }
             p { margin-bottom: 0.5rem; }
         """ ]
