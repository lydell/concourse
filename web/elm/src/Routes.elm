module Routes exposing
    ( DashboardView(..)
    , Highlight(..)
    , Route(..)
    , SearchType(..)
    , StepID
    , Transition
    , buildRoute
    , extractPid
    , extractQuery
    , getGroups
    , jobRoute
    , parsePath
    , pipelineRoute
    , resourceRoute
    , showHighlight
    , toString
    , tokenToFlyRoute
    , versionQueryParams
    , withGroups
    )

import Api.Pagination
import AppUrl exposing (AppUrl, QueryParameters)
import Concourse exposing (InstanceVars, JsonValue(..))
import Concourse.Pagination as Pagination exposing (Direction(..))
import Dict exposing (Dict)
import DotNotation
import Maybe.Extra
import RouteBuilder
import Url


type Route
    = Build { id : Concourse.JobBuildIdentifier, highlight : Highlight, groups : List String }
    | Resource { id : Concourse.ResourceIdentifier, page : Maybe Pagination.Page, version : Maybe Concourse.Version, groups : List String }
    | Job { id : Concourse.JobIdentifier, page : Maybe Pagination.Page, groups : List String }
    | OneOffBuild { id : Concourse.BuildId, highlight : Highlight }
    | Pipeline { id : Concourse.PipelineIdentifier, groups : List String }
    | Dashboard { searchType : SearchType, dashboardView : DashboardView }
    | FlySuccess Bool (Maybe Int)
      -- the version field is really only used as a hack to populate the breadcrumbs, it's not actually used by anyhting else
    | Causality { id : Concourse.VersionedResourceIdentifier, direction : Concourse.CausalityDirection, version : Maybe Concourse.Version, groups : List String }


type SearchType
    = HighDensity
    | Normal String


type DashboardView
    = ViewNonArchivedPipelines
    | ViewAllPipelines


dashboardViewName : DashboardView -> String
dashboardViewName view =
    case view of
        ViewAllPipelines ->
            "all"

        ViewNonArchivedPipelines ->
            "non_archived"


dashboardViewFromString : String -> Maybe DashboardView
dashboardViewFromString view =
    case view of
        "all" ->
            Just ViewAllPipelines

        "non_archived" ->
            Just ViewNonArchivedPipelines

        _ ->
            Nothing


type Highlight
    = HighlightNothing
    | HighlightLine StepID Int
    | HighlightRange StepID Int Int


type alias StepID =
    String


type alias Transition =
    { from : Route
    , to : Route
    }



-- pages


pageFromQueryParameters : QueryParameters -> Maybe Pagination.Page
pageFromQueryParameters queryParameters =
    parsePage
        (getIntParameter "from" queryParameters)
        (getIntParameter "to" queryParameters)
        (getIntParameter "limit" queryParameters)


getIntParameter : String -> QueryParameters -> Maybe Int
getIntParameter name queryParameters =
    Dict.get name queryParameters
        |> Maybe.andThen List.head
        |> Maybe.andThen String.toInt


build :
    Maybe String
    -> Concourse.TeamName
    -> Concourse.PipelineName
    -> Concourse.JobName
    -> Concourse.BuildName
    -> InstanceVars
    -> Route
build fragment teamName pipelineName jobName buildName instanceVars =
    Build
        { id =
            { teamName = teamName
            , pipelineName = pipelineName
            , pipelineInstanceVars = instanceVars
            , jobName = jobName
            , buildName = buildName
            }
        , highlight = parseHighlight fragment
        , groups = []
        }


parsePage : Maybe Int -> Maybe Int -> Maybe Int -> Maybe Pagination.Page
parsePage from to limit =
    case ( from, to, limit ) of
        ( Nothing, Just t, Just l ) ->
            Just
                { direction = Pagination.To t
                , limit = l
                }

        ( Just f, Nothing, Just l ) ->
            Just
                { direction = Pagination.From f
                , limit = l
                }

        _ ->
            Nothing


resource :
    QueryParameters
    -> Concourse.TeamName
    -> Concourse.PipelineName
    -> String
    -> InstanceVars
    -> Route
resource queryParameters teamName pipelineName resourceName instanceVars =
    Resource
        { id =
            { teamName = teamName
            , pipelineName = pipelineName
            , pipelineInstanceVars = instanceVars
            , resourceName = resourceName
            }
        , page = pageFromQueryParameters queryParameters
        , version = resourceVersion queryParameters
        , groups = []
        }


resourceVersion : QueryParameters -> Maybe Concourse.Version
resourceVersion queryParameters =
    let
        split s =
            case String.split ":" s of
                x :: xs ->
                    Just ( x, String.join ":" xs )

                _ ->
                    Nothing

        parse queries =
            List.map split queries |> Maybe.Extra.values |> Dict.fromList

        clean queries =
            if Dict.isEmpty <| parse queries then
                Nothing

            else
                Just <| parse queries
    in
    Dict.get "filter" queryParameters |> Maybe.andThen clean


job :
    QueryParameters
    -> Concourse.TeamName
    -> Concourse.PipelineName
    -> Concourse.JobName
    -> InstanceVars
    -> Route
job queryParameters teamName pipelineName jobName instanceVars =
    Job
        { id =
            { teamName = teamName
            , pipelineName = pipelineName
            , pipelineInstanceVars = instanceVars
            , jobName = jobName
            }
        , page = pageFromQueryParameters queryParameters
        , groups = []
        }


pipeline :
    QueryParameters
    -> Concourse.TeamName
    -> Concourse.PipelineName
    -> InstanceVars
    -> Route
pipeline queryParameters teamName pipelineName instanceVars =
    Pipeline
        { id =
            { teamName = teamName
            , pipelineName = pipelineName
            , pipelineInstanceVars = instanceVars
            }
        , groups = Dict.get "group" queryParameters |> Maybe.withDefault []
        }


dashboard : QueryParameters -> SearchType -> Route
dashboard queryParameters searchType =
    Dashboard
        { searchType = searchType
        , dashboardView = dashboardViewQuery queryParameters
        }


dashboardViewQuery : QueryParameters -> DashboardView
dashboardViewQuery queryParameters =
    Dict.get "view" queryParameters
        |> Maybe.andThen List.head
        |> Maybe.andThen dashboardViewFromString
        |> Maybe.withDefault ViewNonArchivedPipelines


flySuccess : QueryParameters -> Route
flySuccess queryParameters =
    FlySuccess
        (Dict.get "noop" queryParameters |> Maybe.andThen List.head |> (==) (Just "true"))
        (getIntParameter "fly_port" queryParameters)


causality :
    Concourse.TeamName
    -> Concourse.PipelineName
    -> String
    -> Concourse.CausalityDirection
    -> InstanceVars
    -> Int
    -> Route
causality teamName pipelineName resourceName direction instanceVars versionId =
    Causality
        { id =
            { teamName = teamName
            , pipelineName = pipelineName
            , pipelineInstanceVars = instanceVars
            , resourceName = resourceName
            , versionID = versionId
            }
        , direction = direction
        , version = Nothing
        , groups = []
        }



-- route utils


buildRoute : Int -> String -> Maybe Concourse.JobIdentifier -> Route
buildRoute id name jobId =
    case jobId of
        Just j ->
            Build
                { id =
                    { teamName = j.teamName
                    , pipelineName = j.pipelineName
                    , pipelineInstanceVars = j.pipelineInstanceVars
                    , jobName = j.jobName
                    , buildName = name
                    }
                , highlight = HighlightNothing
                , groups = []
                }

        Nothing ->
            OneOffBuild { id = id, highlight = HighlightNothing }


jobRoute : Concourse.Job -> Route
jobRoute j =
    Job
        { id =
            { teamName = j.teamName
            , pipelineName = j.pipelineName
            , pipelineInstanceVars = j.pipelineInstanceVars
            , jobName = j.name
            }
        , page = Nothing
        , groups = []
        }


resourceRoute : Concourse.ResourceIdentifier -> Maybe Concourse.Version -> Route
resourceRoute r v =
    Resource
        { id =
            { teamName = r.teamName
            , pipelineName = r.pipelineName
            , pipelineInstanceVars = r.pipelineInstanceVars
            , resourceName = r.resourceName
            }
        , page = Nothing
        , version = v
        , groups = []
        }


pipelineRoute : { a | name : String, teamName : String, instanceVars : InstanceVars } -> List String -> Route
pipelineRoute p groups =
    Pipeline
        { id = Concourse.toPipelineId p
        , groups = groups
        }


showHighlight : Highlight -> Maybe String
showHighlight hl =
    case hl of
        HighlightNothing ->
            Nothing

        HighlightLine id line ->
            "L" ++ id ++ ":" ++ String.fromInt line |> Just

        HighlightRange id line1 line2 ->
            "L"
                ++ id
                ++ ":"
                ++ String.fromInt line1
                ++ ":"
                ++ String.fromInt line2
                |> Just


parseHighlight : Maybe String -> Highlight
parseHighlight hash =
    case hash of
        Just h ->
            case String.uncons h of
                Just ( 'L', selector ) ->
                    case String.split ":" selector of
                        [ stepID, line1str, line2str ] ->
                            case ( String.toInt line1str, String.toInt line2str ) of
                                ( Just line1, Just line2 ) ->
                                    HighlightRange stepID line1 line2

                                _ ->
                                    HighlightNothing

                        [ stepID, linestr ] ->
                            case String.toInt linestr of
                                Just line ->
                                    HighlightLine stepID line

                                _ ->
                                    HighlightNothing

                        _ ->
                            HighlightNothing

                _ ->
                    HighlightNothing

        _ ->
            HighlightNothing


tokenToFlyRoute : String -> Int -> String
tokenToFlyRoute authToken flyPort =
    "http://127.0.0.1:"
        ++ String.fromInt flyPort
        ++ AppUrl.toString
            { path = []
            , queryParameters = Dict.singleton "token" [ authToken ]
            , fragment = Nothing
            }



-- router


toString : Route -> String
toString =
    toUrl >> AppUrl.toString


toUrl : Route -> AppUrl
toUrl route =
    case route of
        Build { id, highlight } ->
            pipelineIdBuilder id
                { path = [ "jobs", id.jobName, "builds", id.buildName ]
                , queryParameters = Dict.empty
                , fragment = showHighlight highlight
                }

        Job { id, page } ->
            pipelineIdBuilder id
                { path = [ "jobs", id.jobName ]
                , queryParameters = Api.Pagination.params page
                , fragment = Nothing
                }

        Resource { id, page, version } ->
            pipelineIdBuilder id
                { path = [ "resources", id.resourceName ]
                , queryParameters =
                    Dict.union
                        (Api.Pagination.params page)
                        (versionQueryParams (Maybe.withDefault Dict.empty version))
                , fragment = Nothing
                }

        OneOffBuild { id, highlight } ->
            { path = [ "builds", String.fromInt id ]
            , queryParameters = Dict.empty
            , fragment = showHighlight highlight
            }

        Pipeline { id, groups } ->
            pipelineIdBuilder id
                { path = []
                , queryParameters = Dict.singleton "group" groups
                , fragment = Nothing
                }

        Dashboard { searchType, dashboardView } ->
            { path =
                case searchType of
                    Normal _ ->
                        []

                    HighDensity ->
                        [ "hd" ]
            , queryParameters =
                Dict.fromList
                    [ ( "search"
                      , case searchType of
                            Normal "" ->
                                []

                            Normal query ->
                                [ query ]

                            _ ->
                                []
                      )
                    , ( "view"
                      , case dashboardView of
                            ViewNonArchivedPipelines ->
                                []

                            _ ->
                                [ dashboardViewName dashboardView ]
                      )
                    ]
            , fragment = Nothing
            }

        FlySuccess noop flyPort ->
            { path = [ "fly_success" ]
            , queryParameters =
                Dict.fromList
                    [ ( "fly_port", flyPort |> Maybe.map String.fromInt |> Maybe.Extra.toList )
                    , ( "noop"
                      , if noop then
                            [ "true" ]

                        else
                            []
                      )
                    ]
            , fragment = Nothing
            }

        Causality { id, direction } ->
            let
                path =
                    case direction of
                        Concourse.Downstream ->
                            "downstream"

                        Concourse.Upstream ->
                            "upstream"
            in
            pipelineIdBuilder id
                { path = [ "resources", id.resourceName, "causality", String.fromInt id.versionID, path ]
                , queryParameters = Dict.empty
                , fragment = Nothing
                }


parsePath : Url.Url -> Maybe Route
parsePath fullUrl =
    let
        url =
            AppUrl.fromUrl fullUrl

        instanceVars =
            -- This could be rewritten to use `url.queryParameters` instead,
            -- but I didn’t feel like touching the `DotNotation` parser.
            fullUrl.query
                |> Maybe.withDefault ""
                |> String.split "&"
                |> List.filter (\s -> String.startsWith "vars." s || String.startsWith "vars=" s)
                |> List.filterMap Url.percentDecode
                |> List.filterMap (DotNotation.parse >> Result.toMaybe)
                |> DotNotation.expand
                |> Dict.get "vars"
                |> toDict
    in
    case url.path of
        [] ->
            Dict.get "search" url.queryParameters
                |> Maybe.andThen List.head
                |> Maybe.withDefault ""
                |> Normal
                |> dashboard url.queryParameters
                |> Just

        [ "hd" ] ->
            Just (dashboard url.queryParameters HighDensity)

        [ "fly_success" ] ->
            Just (flySuccess url.queryParameters)

        [ "builds", buildIdString ] ->
            String.toInt buildIdString
                |> Maybe.map (\buildId -> OneOffBuild { id = buildId, highlight = parseHighlight url.fragment })

        [ "teams", teamName, "pipelines", pipelineName ] ->
            Just (pipeline url.queryParameters teamName pipelineName instanceVars)

        [ "teams", teamName, "pipelines", pipelineName, "resources", resourceName ] ->
            Just (resource url.queryParameters teamName pipelineName resourceName instanceVars)

        [ "teams", teamName, "pipelines", pipelineName, "resources", resourceName, "causality", versionIdString, "upstream" ] ->
            String.toInt versionIdString
                |> Maybe.map (causality teamName pipelineName resourceName Concourse.Upstream instanceVars)

        [ "teams", teamName, "pipelines", pipelineName, "resources", resourceName, "causality", versionIdString, "downstream" ] ->
            String.toInt versionIdString
                |> Maybe.map (causality teamName pipelineName resourceName Concourse.Downstream instanceVars)

        [ "teams", teamName, "pipelines", pipelineName, "jobs", jobName ] ->
            Just (job url.queryParameters teamName pipelineName jobName instanceVars)

        [ "teams", teamName, "pipelines", pipelineName, "jobs", jobName, "builds", buildName ] ->
            Just (build url.fragment teamName pipelineName jobName buildName instanceVars)

        _ ->
            Nothing


toDict : Maybe JsonValue -> Dict String JsonValue
toDict j =
    case j of
        Just (JsonObject kvs) ->
            Dict.fromList kvs

        _ ->
            Dict.empty



-- route utils


extractPid : Route -> Maybe Concourse.PipelineIdentifier
extractPid route =
    case route of
        Build { id } ->
            Just <| Concourse.pipelineId id

        Job { id } ->
            Just <| Concourse.pipelineId id

        Resource { id } ->
            Just <| Concourse.pipelineId id

        Pipeline { id } ->
            Just id

        _ ->
            Nothing


extractQuery : SearchType -> String
extractQuery route =
    case route of
        Normal q ->
            q

        _ ->
            ""


versionQueryParams : Concourse.Version -> QueryParameters
versionQueryParams version =
    Dict.singleton "filter" (Concourse.versionQuery version)


pipelineIdBuilder : { r | teamName : String, pipelineName : String, pipelineInstanceVars : Concourse.InstanceVars } -> AppUrl -> AppUrl
pipelineIdBuilder id url =
    let
        ( path, query ) =
            RouteBuilder.pipeline id
    in
    { path = path ++ url.path
    , queryParameters = Dict.union url.queryParameters query
    , fragment = url.fragment
    }


getGroups : Route -> List String
getGroups route =
    case route of
        Build { groups } ->
            groups

        Resource { groups } ->
            groups

        Job { groups } ->
            groups

        Pipeline { groups } ->
            groups

        Causality { groups } ->
            groups

        OneOffBuild _ ->
            []

        Dashboard _ ->
            []

        FlySuccess _ _ ->
            []


withGroups : List String -> Route -> Route
withGroups groups route =
    case route of
        Build params ->
            Build { params | groups = groups }

        Resource params ->
            Resource { params | groups = groups }

        Job params ->
            Job { params | groups = groups }

        Pipeline params ->
            Pipeline { params | groups = groups }

        Causality params ->
            Causality { params | groups = groups }

        OneOffBuild _ ->
            route

        Dashboard _ ->
            route

        FlySuccess _ _ ->
            route
