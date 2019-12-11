{--
-- @description: A simple calculator app
-- @authors Alfred ()
-- @date    2018-04-04 15:02:16
-- @version 1.0.0
--}


module Calculator exposing (..)

import Html exposing (Html, a, div, text,button)
import Html.Attributes exposing (class, classList, href, style,attribute)
import Html.Events exposing (onClick)


--MAIN (Html.beginnerProgram)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { firstNumber : String
    , secondNumber : String
    , operation : String
    , pressedOps : Bool
    , pressedDot : Bool
    , pressEqual : Bool
    , soln: String
    }


model : Model
model =
    { firstNumber = ""
    , secondNumber = ""
    , operation = ""
    , pressedOps = False
    , pressedDot = False
    , pressEqual = False
    , soln = ""
    }



-- UPDATE


type Msg
    = Operation String
    | Digit String



-- | PressedOps Bool
-- | PressedDot Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        Operation cmd ->
            -- model
            case cmd of
                "plus" ->
                    if model.pressedOps || model.firstNumber=="" then
                        model
                    else
                        {model| operation = "+", pressedOps = True,  pressedDot = False}
                
                "minus" ->
                    if model.pressedOps || model.firstNumber=="" then
                        model
                    else
                        {model| operation = "-", pressedOps = True,  pressedDot = False}
                "divide" ->
                    if model.pressedOps || model.firstNumber==""  then
                        model
                    else
                        {model| operation = "/", pressedOps = True,  pressedDot = False}
                "times" ->
                    if model.pressedOps || model.firstNumber=="" then
                        model
                    else
                        {model| operation = "*", pressedOps = True,  pressedDot = False}
                
                "clear" ->
                    { firstNumber = ""
                    , secondNumber = ""
                    , operation = ""
                    , pressedOps = False
                    , pressedDot = False
                    , pressEqual = False
                    , soln = ""
                    }
                "equal" ->
                    if model.pressedOps && model.secondNumber /= "" then
                        doEqual model  
                    else
                        model
                "dot" ->
                    if model.pressedDot then
                        model
                    else
                        addDigit "." {model| pressedDot = True}
        --          "delete"->
                _->
                    model
                    
        Digit digit ->
            addDigit digit model
doEqual: Model -> Model
doEqual model=
    let
        fNum = 
            case String.toFloat model.firstNumber of
                Err message ->
                    0
                Ok num ->
                    num
        sNum =
            case String.toFloat model.secondNumber of
                Err message ->
                    0
                Ok num ->
                    num
        ans = 
            case model.operation of
                "+" ->
                    sNum + fNum
                "-" ->
                    fNum - sNum
                "/"->
                    fNum / sNum
                "*" ->
                    fNum * sNum
                _ ->
                    0

    in
        {model| soln = toString ans, pressEqual = True}




addDigit : String -> Model -> Model
addDigit digit model =
    if not model.pressEqual then
        let
            fNum= model.firstNumber
            sNum = model.secondNumber
        in
        if model.pressedOps then
            {model| secondNumber = sNum ++ digit}
        else
            {model| firstNumber = fNum ++ digit}
    else
        model



-- VIEW


view : Model -> Html Msg
view model =
    let
        ans = 
           if model.soln == "" then
                model.firstNumber ++" "++ model.operation ++" "++model.secondNumber
            else
                model.firstNumber ++" "++ model.operation ++" "++model.secondNumber ++" = "++model.soln
    in
        div [ class "section", style [ ( "width", "500px" ) ] ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ div [ class "column is-half box", style [ ( "background-color", "#5f9ea0" ) ] ]
                        [ div [ class "box has-text-right" ]
                            [ text ans ]
                        , div [ class "box", style [ ( "background-color", "#f8f8ff" ) ] ]
                            [ div [ class "columns" ]
                                [ div [ class "column" ]
                                    [ div [ class "columns" ]
                                        [ div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined" , onClick ( Operation "plus")]
                                                [ text "+" ]
                                            ]
                                        , div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined", onClick ( Operation "minus") ]
                                                [ text "-" ]
                                            ]
                                        ]
                                    , div [ class "columns" ]
                                        [ div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined" , onClick ( Operation "times")]
                                                [ text "*" ]
                                            ]
                                        , div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined", onClick ( Operation "divide") ]
                                                [ text "/" ]
                                            ]
                                        ]
                                    , div [ class "columns" ]
                                        [ div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined", onClick ( Operation "clear") ]
                                                [ text "clear" ]
                                            ]
                                        , div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined" , onClick ( Operation "delete")]
                                                [ text "delete" ]
                                            ]
                                        ]
                                    ]
                                , div [ class "column" ]
                                    [ div [ class "columns" ]
                                        [ div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined", onClick ( Digit "1") ]
                                                [ text "1" ]
                                            ]
                                        , div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined", onClick ( Digit "2") ]
                                                [ text "2" ]
                                            ]
                                        , div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined" , onClick ( Digit "3")]
                                                [ text "3" ]
                                            ]
                                        ]
                                    , div [ class "columns" ]
                                        [ div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined", onClick ( Digit "4") ]
                                                [ text "4" ]
                                            ]
                                        , div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined", onClick ( Digit "5") ]
                                                [ text "5" ]
                                            ]
                                        , div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined", onClick ( Digit "6") ]
                                                [ text "6" ]
                                            ]
                                        ]
                                    , div [ class "columns" ]
                                        [ div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined", onClick ( Digit "7") ]
                                                [ text "7" ]
                                            ]
                                        , div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined" , onClick ( Digit "8")]
                                                [ text "8" ]
                                            ]
                                        , div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined" , onClick ( Digit "9")]
                                                [ text "9" ]
                                            ]
                                        ]
                                    , div [ class "columns" ]
                                        [ div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined" , onClick ( Digit "0")]
                                                [ text "0" ]
                                            ]
                                        , div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined" , onClick ( Operation "dot")]
                                                [ text "." ]
                                            ]
                                        , div [ class "column" ]
                                            [ button [ class "button is-primary is-outlined", onClick ( Operation "equal") ]
                                                [ text "=" ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
    