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


pipelineIdentifier : AppUrl -> ({ teamName : String, pipelineName : String } -> List String -> Maybe a) -> Maybe a
pipelineIdentifier url f =
    case url.path of
        "teams" :: teamName :: "pipelines" :: pipelineName :: rest ->
            f { teamName = teamName, pipelineName = pipelineName } rest

        _ ->
            Nothing


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


build : AppUrl -> Maybe (InstanceVars -> Route)
build url =
    let
        buildHelper { teamName, pipelineName } jobName buildName h =
            \iv ->
                Build
                    { id =
                        { teamName = teamName
                        , pipelineName = pipelineName
                        , pipelineInstanceVars = iv
                        , jobName = jobName
                        , buildName = buildName
                        }
                    , highlight = h
                    , groups = []
                    }
    in
    pipelineIdentifier url <|
        \identifier rest ->
            case rest of
                [ "jobs", jobName, "builds", buildName ] ->
                    Just (buildHelper identifier jobName buildName (parseHighlight url.fragment))

                _ ->
                    Nothing


oneOffBuild : AppUrl -> Maybe Route
oneOffBuild url =
    case url.path of
        [ "builds", buildIdString ] ->
            String.toInt buildIdString
                |> Maybe.map (\buildId -> OneOffBuild { id = buildId, highlight = parseHighlight url.fragment })

        _ ->
            Nothing


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


resource : AppUrl -> Maybe (InstanceVars -> Route)
resource url =
    let
        resourceHelper { teamName, pipelineName } resourceName page version =
            \iv ->
                Resource
                    { id =
                        { teamName = teamName
                        , pipelineName = pipelineName
                        , pipelineInstanceVars = iv
                        , resourceName = resourceName
                        }
                    , page = page
                    , version = version
                    , groups = []
                    }
    in
    pipelineIdentifier url <|
        \identifier rest ->
            case rest of
                [ "resources", resourceName ] ->
                    resourceHelper
                        identifier
                        resourceName
                        (pageFromQueryParameters url.queryParameters)
                        (resourceVersion url.queryParameters)
                        |> Just

                _ ->
                    Nothing


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


job : AppUrl -> Maybe (InstanceVars -> Route)
job url =
    let
        jobHelper { teamName, pipelineName } jobName page =
            \iv ->
                Job
                    { id =
                        { teamName = teamName
                        , pipelineName = pipelineName
                        , pipelineInstanceVars = iv
                        , jobName = jobName
                        }
                    , page = page
                    , groups = []
                    }
    in
    pipelineIdentifier url <|
        \identifier rest ->
            case rest of
                [ "jobs", jobName ] ->
                    Just (jobHelper identifier jobName (pageFromQueryParameters url.queryParameters))

                _ ->
                    Nothing


pipeline : AppUrl -> Maybe (InstanceVars -> Route)
pipeline url =
    let
        pipelineHelper { teamName, pipelineName } g =
            \iv ->
                Pipeline
                    { id =
                        { teamName = teamName
                        , pipelineName = pipelineName
                        , pipelineInstanceVars = iv
                        }
                    , groups = g
                    }
    in
    pipelineIdentifier url <|
        \identifier rest ->
            case rest of
                [] ->
                    Just (pipelineHelper identifier (Dict.get "group" url.queryParameters |> Maybe.withDefault []))

                _ ->
                    Nothing


dashboard : AppUrl -> Maybe Route
dashboard url =
    let
        dashboardHelper searchType =
            Dashboard
                { searchType = searchType
                , dashboardView = dashboardViewQuery url.queryParameters
                }
    in
    case url.path of
        [] ->
            Dict.get "search" url.queryParameters
                |> Maybe.andThen List.head
                |> Maybe.withDefault ""
                |> Normal
                |> dashboardHelper
                |> Just

        [ "hd" ] ->
            Just (dashboardHelper HighDensity)

        _ ->
            Nothing


dashboardViewQuery : QueryParameters -> DashboardView
dashboardViewQuery queryParameters =
    Dict.get "view" queryParameters
        |> Maybe.andThen List.head
        |> Maybe.andThen dashboardViewFromString
        |> Maybe.withDefault ViewNonArchivedPipelines


flySuccess : AppUrl -> Maybe Route
flySuccess url =
    case url.path of
        [ "fly_success" ] ->
            FlySuccess
                (Dict.get "noop" url.queryParameters |> Maybe.andThen List.head |> (==) (Just "true"))
                (getIntParameter "fly_port" url.queryParameters)
                |> Just

        _ ->
            Nothing


causality : AppUrl -> Maybe (InstanceVars -> Route)
causality url =
    let
        causalityHelper { teamName, pipelineName } resourceName direction versionId =
            \iv ->
                Causality
                    { id =
                        { teamName = teamName
                        , pipelineName = pipelineName
                        , pipelineInstanceVars = iv
                        , resourceName = resourceName
                        , versionID = versionId
                        }
                    , direction = direction
                    , version = Nothing
                    , groups = []
                    }
    in
    pipelineIdentifier url <|
        \identifier rest ->
            case rest of
                [ "resources", resourceName, "causality", versionIdString, directionString ] ->
                    let
                        direction =
                            case directionString of
                                "upstream" ->
                                    Just Concourse.Upstream

                                "downstream" ->
                                    Just Concourse.Downstream

                                _ ->
                                    Nothing
                    in
                    Maybe.map2 (causalityHelper identifier resourceName)
                        direction
                        (String.toInt versionIdString)

                _ ->
                    Nothing



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


sitemap : AppUrl -> Maybe (InstanceVars -> Route)
sitemap url =
    oneOf url
        [ resource
        , job
        , dashboard >> Maybe.map always
        , pipeline
        , build
        , oneOffBuild >> Maybe.map always
        , flySuccess >> Maybe.map always
        , causality
        ]


oneOf : AppUrl -> List (AppUrl -> Maybe a) -> Maybe a
oneOf url list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            case first url of
                Just a ->
                    Just a

                Nothing ->
                    oneOf url rest


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
    sitemap url |> Maybe.map (\deferredRoute -> deferredRoute instanceVars)


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
