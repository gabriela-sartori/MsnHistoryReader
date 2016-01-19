import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)
import Signal          exposing (..)
import Json.Decode     exposing (..)
import Json.Encode
import Result          exposing (..)
import String          exposing (cons, uncons, fromList)
import VirtualDom      exposing (property)

type Action = ActNope | ActUpdateJson String | ActUpdateChk1 Bool | ActUpdateChk2 Bool
type alias CustomText = { style : String, text : String }
type alias Message    = { from : String, to : String, text : CustomText }
type alias Model      = { messages                  : List Message
                        , json                      : String
                        , ignore_msn_plus_name_tags : Bool
                        , remove_text_font          : Bool
                        }

initialModel : Model
initialModel = Model [] ":D" True False

update : Action -> Model -> Model
update action model =
    case action of
        ActNope -> model
        ActUpdateJson str -> { model | json = str }
        ActUpdateChk1 b -> { model | ignore_msn_plus_name_tags = b }
        ActUpdateChk2 b -> { model | remove_text_font = b }
        
update_messages : Model -> Model
update_messages model =
    { model |
        messages = decodeString decodeMsn model.json
                |> Result.toMaybe
                |> Maybe.withDefault [ Message "Erro" "ErroTo" (CustomText "" "Error parsing the json :(") ]
    }

decodeMsn : Decoder (List Message)
decodeMsn =
    let
        custom_text = object2 CustomText ( at ["_Style"] string )
                                         ( at ["__text"] string )
        message     = object3 Message ( at ["From", "User", "_FriendlyName"] string )
                                      ( at ["To",   "User", "_FriendlyName"] string )
                                      ( at ["Text"] custom_text )
    in
        at ["Log", "Message"] (Json.Decode.list message)

remove_tags : String -> String
remove_tags str =
    let
        removeTags' : String -> Bool -> String
        removeTags' str add =
            case uncons str of
                Just ('[', str) -> removeTags' str False
                Just (']', str) -> removeTags' str True
                Just (c, str)   -> (fromList (if add then [c] else [])) ++ removeTags' str add
                Nothing         -> ""
    in
        removeTags' str True

msg_to_div : Bool -> Bool -> Message -> Html
msg_to_div remove_tag remove_font msg =
    div [] [ b [] [ text ((if remove_tag then remove_tags else identity) msg.from) ]
           , br [] []
           , if not remove_font then
                 span [ Html.Attributes.property "style" (Json.Encode.string msg.text.style) ] [ text msg.text.text ]
             else
                 text msg.text.text
           ]

view : Signal.Address Action -> Model -> Html
view address model =
    div [] ([ text "Informe aqui o json:"
            , br [] []
            , textarea [ rows 10, cols 50, on "input" targetValue (ActUpdateJson >> Signal.message address) ] [ text model.json ]
            , br [] []
            , input [ type' "checkbox"
                    , checked model.ignore_msn_plus_name_tags
                    , on "change" targetChecked (Signal.message address << ActUpdateChk1)
                    ] [], text "Ignore msn plus name tags"
            , br [] []
            , input [ type' "checkbox"
                    , checked model.remove_text_font
                    , on "change" targetChecked (Signal.message address << ActUpdateChk2)
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

main : Signal Html
main =
    let
        mailbox : Signal.Mailbox Action
        mailbox = Signal.mailbox ActNope
        model_signal : Signal Model
        model_signal = Signal.foldp (\action model -> update_messages <| update action model) initialModel mailbox.signal
    in
        Signal.map (view mailbox.address) model_signal
