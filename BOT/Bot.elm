{- EVE Online Intel Bot - Local Watch Script - 2021-09-21
   This bot watches local and plays an alarm sound when a pilot with bad standing appears.
-}
{-
   bot-catalog-tags:eve-online,intel,local-watch
   authors-forum-usernames:viir
-}


module Bot exposing
    ( State
    , botMain
    )

import BotLab.BotInterface_To_Host_20210823 as InterfaceToHost
import Common.AppSettings as AppSettings
import Common.EffectOnWindow exposing (MouseButton(..))
import Dict
import EveOnline.BotFramework
    exposing
        ( BotEventResponseEffect(..)
        , ReadingFromGameClient
        , UseContextMenuCascadeNode(..)
        )


type alias BotState =
    {}


type alias State =
    EveOnline.BotFramework.StateIncludingFramework BotSettings BotState


type alias BotSettings =
    { monitoredShipsPatterns : List String }


defaultBotSettings : BotSettings
defaultBotSettings =
    { monitoredShipsPatterns = [ "Shuttle" ] }


parseBotSettings : String -> Result String BotSettings
parseBotSettings =
    AppSettings.parseSimpleListOfAssignments { assignmentsSeparators = [ ",", "\n" ] }
        ([ ( "monitored-ships-patterns"
           , AppSettings.valueTypeString
                (\shipsPatterns settings ->
                    { settings | monitoredShipsPatterns = shipsPatterns |> String.split "," |> List.map String.trim }
                )
           )
         ]
            |> Dict.fromList
        )
        defaultBotSettings


botMain : InterfaceToHost.BotConfig State
botMain =
    { init = initState
    , processEvent = processEvent
    }


initState : State
initState =
    EveOnline.BotFramework.initState {}


processEvent : InterfaceToHost.BotEvent -> State -> ( State, InterfaceToHost.BotEventResponse )
processEvent =
    EveOnline.BotFramework.processEvent
        { parseBotSettings = parseBotSettings
        , selectGameClientInstance = always EveOnline.BotFramework.selectGameClientInstanceWithTopmostWindow
        , processEvent = processEveOnlineBotEvent
        }


processEveOnlineBotEvent :
    EveOnline.BotFramework.BotEventContext BotSettings
    -> EveOnline.BotFramework.BotEvent
    -> BotState
    -> ( BotState, EveOnline.BotFramework.BotEventResponse )
processEveOnlineBotEvent eventContext event stateBefore =
    case event of
        EveOnline.BotFramework.ReadingFromGameClientCompleted parsedUserInterface _ ->
            let
                effects =
                    botEffectsFromGameClientState eventContext.botSettings parsedUserInterface
            in
            ( stateBefore
            , EveOnline.BotFramework.ContinueSession
                { effects = effects
                , millisecondsToNextReadingFromGame = 2000
                , statusDescriptionText = ""
                , screenshotRegionsToRead = always { rects1x1 = [] }
                }
            )


botEffectsFromGameClientState : BotSettings -> ReadingFromGameClient -> List BotEventResponseEffect
botEffectsFromGameClientState settings parsedUserInterface =
    let
        alarmRequests =
            case parsedUserInterface.overviewWindow of
                Nothing ->
                    [ EveOnline.BotFramework.EffectConsoleBeepSequence
                        [ { frequency = 700, durationInMs = 100 }
                        , { frequency = 0, durationInMs = 100 }
                        , { frequency = 600, durationInMs = 1000 }
                        ]
                    ]

                Just overviewWindow ->
                    let
                        matchingOverviewEntries =
                            overviewWindow.entries
                                |> List.filter
                                    (\entry ->
                                        settings.monitoredShipsPatterns
                                            |> List.any
                                                (\pattern ->
                                                    String.contains
                                                        (String.toLower pattern)
                                                        (String.toLower (Maybe.withDefault "" entry.objectType))
                                                )
                                    )
                    in
                    if 0 < (matchingOverviewEntries |> List.length) then
                        [ EveOnline.BotFramework.EffectConsoleBeepSequence
                            [ { frequency = 700, durationInMs = 100 }
                            , { frequency = 0, durationInMs = 100 }
                            , { frequency = 700, durationInMs = 500 }
                            ]
                        ]

                    else
                        []
    in
    alarmRequests
