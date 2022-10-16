module RouteBuilder exposing (RouteBuilder, append, appendPath, appendQuery, build, pipeline)

import AppUrl exposing (QueryParameters)
import Concourse
import Dict
import DotNotation


type alias RouteBuilder =
    ( List String, QueryParameters )


append : RouteBuilder -> RouteBuilder -> RouteBuilder
append ( p1, q1 ) ( p2, q2 ) =
    ( p2 ++ p1, Dict.union q2 q1 )


appendPath : List String -> RouteBuilder -> RouteBuilder
appendPath path base =
    append ( path, Dict.empty ) base


appendQuery : QueryParameters -> RouteBuilder -> RouteBuilder
appendQuery query base =
    append ( [], query ) base


build : RouteBuilder -> String
build ( path, query ) =
    AppUrl.toString
        { path = path
        , queryParameters = query
        , fragment = Nothing
        }


pipeline : { r | teamName : String, pipelineName : String, pipelineInstanceVars : Concourse.InstanceVars } -> RouteBuilder
pipeline id =
    ( [ "teams", id.teamName, "pipelines", id.pipelineName ]
    , DotNotation.flatten id.pipelineInstanceVars
        |> List.map
            (\var ->
                let
                    ( k, v ) =
                        DotNotation.serialize var
                in
                ( "vars." ++ k, [ v ] )
            )
        |> Dict.fromList
    )
