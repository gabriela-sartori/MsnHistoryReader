import Html            exposing (..)
  
import Html.Attributes exposing (..)
import Html.Events     exposing (..)
import Json.Decode     exposing (..)
import Json.Encode
import Result          exposing (..)
import String          exposing (cons, uncons, fromList)

type Action = ActNope | ActUpdateJson String | ActUpdateChk1 Bool | ActUpdateChk2 Bool
type alias CustomText = { style : String, text : String }
type alias Message    = { from : String, to : String, text : CustomText, date : String, time : String }
type alias Model      = { messages                  : List Message
                        , json                      : String
                        , ignore_msn_plus_name_tags : Bool
                        , remove_text_font          : Bool
                        }

update : Action -> Model -> (Model, Cmd Action)
update action model =
    case action of
        ActNope -> model ! []
        ActUpdateJson str -> update_messages { model | json = str } ! []
        ActUpdateChk1 b   -> { model | ignore_msn_plus_name_tags = b } ! []
        ActUpdateChk2 b   -> { model | remove_text_font = b } ! []
        
update_messages : Model -> Model
update_messages model =
    { model |
        messages = decodeString decodeMsn model.json
                |> Result.toMaybe
                |> Maybe.withDefault [ Message "Erro" "ErroTo" (CustomText "" "Error parsing the json :(") "" "" ]
    }

decodeMsn : Decoder (List Message)
decodeMsn =
    let
        custom_text = Json.Decode.map2 (\s t -> CustomText s (Maybe.withDefault "" t))
                                         ( field "_Style" string )
                                         ( maybe <| field "__text" string )
        message     = Json.Decode.map5 Message ( at ["From", "User", "_FriendlyName"] string )
                                      ( at ["To",   "User", "_FriendlyName"] string )
                                      (field "Text"   custom_text )
                                      (field "_Date"  string )
                                      (field "_Time"  string )
    in
        at ["Log", "Message"] (Json.Decode.list message)

remove_tags : String -> String
remove_tags str =
    let
        removeTags_ : String -> Bool -> String
        removeTags_ str add =
            case uncons str of
                Just ('[', str) -> removeTags_ str False
                Just (']', str) -> removeTags_ str True
                Just (c, str)   -> (fromList (if add then [c] else [])) ++ removeTags_ str add
                Nothing         -> ""
    in
        removeTags_ str True

msg_to_div : Bool -> Bool -> Message -> Html Action
msg_to_div remove_tag remove_font msg =
    div [] [ b [] [ text ((if remove_tag then remove_tags else identity) msg.from) ]
           , text <| "  (" ++ msg.date ++ " - " ++ msg.time ++ ")"
           , br [] []
           , if not remove_font then
                 span [ Html.Attributes.property "style" (Json.Encode.string msg.text.style) ] [ text msg.text.text ]
             else
                 text msg.text.text
           ]

view : Model -> Html Action
view model =
    div [] ([ text "The original format is XML. Convert to JSON and paste here:"
            , br [] []
            , textarea [ rows 10, cols 50, onInput ActUpdateJson ] [ text model.json ]
            , br [] []
            , input [ type_ "checkbox"
                    , checked model.ignore_msn_plus_name_tags
                    , onCheck ActUpdateChk1
                    ] [], text "Ignore msn plus name tags"
            , br [] []
            , input [ type_ "checkbox"
                    , checked model.remove_text_font
                    , onCheck ActUpdateChk2
                    ] [], text "Remove font from messages"
            , br [] []
            , br [] []
            , br [] []
            ] ++
            if List.length model.messages > 0 then
                [
                  div [] [ h2 [] [ text "Messages:" ]
                         , (List.map (msg_to_div model.ignore_msn_plus_name_tags model.remove_text_font) model.messages)
                           |> List.intersperse (br [] [])
                           |> div []
                         ]
                ]
            else [])

main =
    Html.program
        { init          = Model [] ":D" True False ! []
        , view          = view
        , update        = update
        , subscriptions = always Sub.none }
