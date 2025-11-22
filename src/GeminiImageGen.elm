module GeminiImageGen exposing
    ( Config
    , GenerationResult
    , generateFromImageAndText
    , responseDecoder
    )

{-| Module for generating images using Google's Gemini API

This module provides integration with Google's Gemini API (generativelanguage.googleapis.com)
to generate images from text prompts or edit images with text instructions.

Uses the gemini-2.0-flash-exp model which supports:

  - Text-to-image generation
  - Image-to-image editing with text prompts
  - Background replacement

API documentation: <https://ai.google.dev/gemini-api/docs/vision>

-}

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra


{-| Configuration for Gemini API
-}
type alias Config =
    { apiKey : String
    }


{-| Result from image generation
-}
type alias GenerationResult =
    { images : List String -- Base64-encoded images
    , error : Maybe String
    }


{-| Generate a background for a portrait image (image + text prompt)

The Gemini API is simple - just provide the base image and a text prompt.
No parameters like guidance scale or seed needed.

    generateBackground
        { apiKey = "AIzaSy..." }
        "data:image/png;base64,iVBORw0KG..."
        "Create a tropical beach background with palm trees at sunset"
        GotGenerationResult

-}
generateFromImageAndText :
    Config
    -> List String
    -> String
    -> (Result ( Http.Error, String ) GenerationResult -> msg)
    -> Cmd msg
generateFromImageAndText config base64Images prompt toMsg =
    let
        url =
            buildApiUrl config

        body =
            buildRequestBody base64Images prompt
                |> Http.jsonBody
    in
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Content-Type" "application/json"
            ]
        , url = url
        , body = body
        , expect = expectJsonWithBody toMsg responseDecoder
        , timeout = Just 60000 -- 60 seconds
        , tracker = Nothing
        }


{-| Custom expect that captures the response body even on errors
-}
expectJsonWithBody : (Result ( Http.Error, String ) a -> msg) -> Decode.Decoder a -> Http.Expect msg
expectJsonWithBody toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err ( Http.BadUrl url, "" )

                Http.Timeout_ ->
                    Err ( Http.Timeout, "" )

                Http.NetworkError_ ->
                    Err ( Http.NetworkError, "" )

                Http.BadStatus_ metadata body ->
                    Err ( Http.BadStatus metadata.statusCode, body )

                Http.GoodStatus_ _ body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err ( Http.BadBody (Decode.errorToString err), body )


{-| Build the Gemini API URL
-}
buildApiUrl : Config -> String
buildApiUrl config =
    "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-image:generateContent?key=" ++ config.apiKey


{-| Build the request body for image generation using Gemini API format

The Gemini API uses a simple structure with "contents" and "parts"
No special parameters - just the prompt and image data

-}
buildRequestBody : List String -> String -> Encode.Value
buildRequestBody base64Images prompt =
    let
        -- Extract just the base64 data without the data URL prefix
        cleanBase64 input =
            if String.startsWith "data:" input then
                input
                    |> String.split ","
                    |> List.drop 1
                    |> String.join ","

            else
                input

        -- Determine MIME type from data URL or default to jpeg
        mimeType input =
            if String.contains "image/png" input then
                "image/png"

            else if String.contains "image/jpeg" input || String.contains "image/jpg" input then
                "image/jpeg"

            else
                "image/jpeg"

        parts =
            [ Encode.object
                [ ( "text", Encode.string prompt )
                ]
            , Encode.list Encode.object
                (base64Images
                    |> List.map
                        (\image ->
                            [ ( "inline_data"
                              , Encode.object
                                    [ ( "mime_type", Encode.string (mimeType image) )
                                    , ( "data", Encode.string (cleanBase64 image) )
                                    ]
                              )
                            ]
                        )
                )
            ]

        contents =
            Encode.list identity
                [ Encode.object
                    [ ( "parts", Encode.list identity parts )
                    ]
                ]
    in
    Encode.object
        [ ( "contents", contents )
        , ( "generationConfig"
          , Encode.object
                [ ( "responseModalities", Encode.list Encode.string [ "Image" ] ) ]
          )
        ]


{-| Decode the Gemini API response
-}
responseDecoder : Decode.Decoder GenerationResult
responseDecoder =
    Decode.oneOf
        [ -- Success case
          Decode.map2 GenerationResult
            (Decode.field "candidates" (Decode.list candidateDecoder))
            (Decode.succeed Nothing)
        , -- Error case
          Decode.map2 GenerationResult
            (Decode.succeed [])
            (Decode.at [ "error", "message" ] (Decode.maybe Decode.string))
        ]


{-| Decode a single candidate (generated image)
-}
candidateDecoder : Decode.Decoder String
candidateDecoder =
    Decode.at [ "content", "parts" ] (Decode.list partDecoder |> Decode.map Maybe.Extra.values)
        |> Decode.andThen
            (\parts ->
                case parts of
                    first :: _ ->
                        Decode.succeed first

                    [] ->
                        Decode.fail "No parts in candidate"
            )


{-| Decode a part that contains inline\_data
-}
partDecoder : Decode.Decoder (Maybe String)
partDecoder =
    Decode.oneOf
        [ Decode.at [ "inlineData", "data" ] Decode.string
            |> Decode.andThen
                (\base64 ->
                    Decode.at [ "inlineData", "mimeType" ] Decode.string
                        |> Decode.map
                            (\mimeType ->
                                Just ("data:" ++ mimeType ++ ";base64," ++ base64)
                            )
                )
        , Decode.succeed Nothing
        ]
