module Concourse.Cli exposing (Cli(..), clis, downloadUrl, id, label)

import Api.Endpoints as Endpoints
import Dict


clis : List Cli
clis =
    [ OSX, Windows, Linux ]


type Cli
    = OSX
    | Windows
    | Linux


downloadUrl : Cli -> String
downloadUrl cli =
    let
        platformName =
            case cli of
                OSX ->
                    "darwin"

                Windows ->
                    "windows"

                Linux ->
                    "linux"
    in
    Endpoints.Cli
        |> Endpoints.toString
            (Dict.fromList
                [ ( "arch", [ "amd64" ] )
                , ( "platform", [ platformName ] )
                ]
            )


label : Cli -> String
label cli =
    let
        platformName =
            case cli of
                OSX ->
                    "OS X"

                Windows ->
                    "Windows"

                Linux ->
                    "Linux"
    in
    "Download " ++ platformName ++ " CLI"


id : Cli -> String
id cli =
    case cli of
        OSX ->
            "osx"

        Windows ->
            "windows"

        Linux ->
            "linux"
