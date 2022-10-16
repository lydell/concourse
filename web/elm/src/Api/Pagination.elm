module Api.Pagination exposing (params, parseLinks, parsePagination)

import AppUrl exposing (QueryParameters)
import Concourse.Pagination
    exposing
        ( Direction(..)
        , Page
        , Paginated
        , Pagination
        )
import Dict
import Http
import Json.Decode
import List.Extra
import Maybe.Extra exposing (orElse)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , backtrackable
        , chompWhile
        , getChompedString
        , keyword
        , oneOf
        , run
        , spaces
        , succeed
        , symbol
        )
import String
import Url


params : Maybe Page -> QueryParameters
params p =
    case p of
        Just { direction, limit } ->
            (case direction of
                From from ->
                    Dict.singleton "from" [ String.fromInt from ]

                To to ->
                    Dict.singleton "to" [ String.fromInt to ]

                ToMostRecent ->
                    Dict.empty
            )
                |> Dict.insert "limit" [ String.fromInt limit ]

        Nothing ->
            Dict.empty


parsePagination :
    Json.Decode.Decoder a
    -> Http.Response String
    -> Result String (Paginated a)
parsePagination decoder response =
    response.body
        |> Json.Decode.decodeString (Json.Decode.list decoder)
        |> Result.mapError Json.Decode.errorToString
        |> Result.map
            (\content ->
                { content = content, pagination = parseLinks response }
            )


parseLinks : Http.Response String -> Pagination
parseLinks =
    .headers
        >> Dict.toList
        >> List.Extra.find (Tuple.first >> String.toLower >> (==) "link")
        >> Maybe.map Tuple.second
        >> Maybe.andThen (run pagination >> Result.toMaybe)
        >> Maybe.withDefault { previousPage = Nothing, nextPage = Nothing }


pagination : Parser Pagination
pagination =
    let
        entry rel =
            backtrackable <|
                succeed parsePage
                    |. symbol "<"
                    |= getChompedString (chompWhile <| (/=) '>')
                    |. symbol ">"
                    |. symbol ";"
                    |. spaces
                    |. keyword "rel"
                    |. symbol "="
                    |. symbol "\""
                    |. keyword rel
                    |. symbol "\""
    in
    oneOf
        [ succeed (\p n -> { previousPage = p, nextPage = n })
            |= entry previousRel
            |. symbol ","
            |. spaces
            |= entry nextRel
        , succeed (\n p -> { previousPage = p, nextPage = n })
            |= entry nextRel
            |. symbol ","
            |. spaces
            |= entry previousRel
        , succeed (\p -> { previousPage = p, nextPage = Nothing })
            |= entry previousRel
        , succeed (\n -> { previousPage = Nothing, nextPage = n })
            |= entry nextRel
        ]


previousRel : String
previousRel =
    "previous"


nextRel : String
nextRel =
    "next"


parsePage : String -> Maybe Page
parsePage urlString =
    let
        tryParam param =
            urlString
                |> Url.fromString
                |> Maybe.andThen
                    (\fullUrl ->
                        let
                            url =
                                AppUrl.fromUrl fullUrl
                        in
                        Dict.get param url.queryParameters
                            |> Maybe.andThen List.head
                            |> Maybe.andThen String.toInt
                    )

        tryDirection dir =
            tryParam
                >> Maybe.map
                    (\n ->
                        { direction = dir n
                        , limit = tryParam "limit" |> Maybe.withDefault 0
                        }
                    )
    in
    tryDirection From "from" |> orElse (tryDirection To "to")
