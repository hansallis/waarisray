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
    { rayTelegramId = 1411086737 -- Replace with Ray's actual Telegram ID
    , appName = "Where is Ray?"
    , defaultMapZoom = 2
    , maxGuessesPerRound = 1
    }


botToken =
    "8075877814:AAHWMuUZsbM-9aBUpCmf_t-SKMzE9FzB5JU"



-- Telegram configuration


telegramConfig :
    { botUsername : String
    , botToken : String
    , webAppUrl : String
    , code15id : String
    }
telegramConfig =
    { botUsername = "whereisraybot" -- Replace with your bot username
    , botToken = botToken -- Replace with your actual bot token from @BotFather
    , code15id = "-1001958086768"
    , webAppUrl =
        case mode of
            Development ->
                "http://localhost:8000"

            Production ->
                "https://waarisray.lamdera.app"

    -- Replace with your actual Lamdera URL
    }



-- Gemini API configuration


geminiApiKey =
    "your-gemini-api-key"


geminiConfig :
    { apiKey : String
    }
geminiConfig =
    { apiKey = geminiApiKey
    }



-- Google Maps API configuration


googleMapsApiKey =
    ""


googleMapsConfig :
    { apiKey : String
    }
googleMapsConfig =
    { apiKey = googleMapsApiKey
    }
