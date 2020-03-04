module Utils exposing (onCustomClick)

import Html
import Html.Events exposing (custom)
import Json.Decode exposing (succeed)


onCustomClick : msg -> Html.Attribute msg
onCustomClick msg =
    custom "click"
        (succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )
