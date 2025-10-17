module Env exposing (..)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.
-- Application Configuration


type Mode
    = Development
    | Production


mode =
    Development



-- Default app settings


appConfig :
    { rayTelegramId : Int
    , appName : String
    , defaultMapZoom : Int
    , maxGuessesPerRound : Int
    }
appConfig =
    { rayTelegramId = 123456789 -- Replace with Ray's actual Telegram ID
    , appName = "Where is Ray?"
    , defaultMapZoom = 2
    , maxGuessesPerRound = 1
    }



-- Determine if we're in development mode


isDevelopment : Bool
isDevelopment =
    -- In a real app, this would be determined by environment variables
    True



-- Get current mode


currentMode : Mode
currentMode =
    if isDevelopment then
        Development

    else
        Production



-- Telegram configuration


telegramConfig :
    { botUsername : String
    , botToken : String
    , webAppUrl : String
    }
telegramConfig =
    { botUsername = "whereisraybot" -- Replace with your bot username
    , botToken = "8075877814:AAHWMuUZsbM-9aBUpCmf_t-SKMzE9FzB5JU" -- Replace with your actual bot token from @BotFather
    , webAppUrl =
        case currentMode of
            Development ->
                "http://localhost:8000"

            Production ->
                "https://waarisray.lamdera.app"

    -- Replace with your actual Lamdera URL
    }



-- Map configuration


mapConfig :
    { defaultCenter : { lat : Float, lng : Float }
    , defaultZoom : Int
    , tileLayerUrl : String
    , attribution : String
    }
mapConfig =
    { defaultCenter = { lat = 51.5074, lng = -0.1278 } -- London
    , defaultZoom = 2
    , tileLayerUrl = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
    , attribution = "© OpenStreetMap contributors"
    }
