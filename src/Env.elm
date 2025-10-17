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
        case mode of
            Development ->
                "http://localhost:8000"

            Production ->
                "https://waarisray.lamdera.app"

    -- Replace with your actual Lamdera URL
    }
