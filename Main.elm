module Main exposing (find_verse, initialCmd, main)

import Browser
import Html exposing (a, button, div, h1, h2, img, input, li, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Parser
import Html.Parser.Util
import Http
import Json.Encode exposing (string)
import Regex


type Msg
    = GotData (Result Http.Error String)
    | GotRubrics (Result Http.Error String)
    | GotVerses (Result Http.Error String)
    | GotSearchVerses (Result Http.Error String)
    | GetVerses String Int
    | GetVerse String String
    | GotVerse (Result Http.Error String)
    | BackToRubric
    | Search String
    | Home


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "https://corsproxy.io/?https://stihi.ru/avtor/smikhno"
        , expect = Http.expectString GotData
        }


initialSearchCmd : Cmd Msg
initialSearchCmd =
    Http.get
        { url = "https://corsproxy.io/?https://stihi.ru/avtor/smikhno"
        , expect = Http.expectString GotRubrics
        }


insert_page_number : String -> Int -> String
insert_page_number url page_number =
    let
        length =
            String.length url

        indexes =
            String.indexes "book" url

        new_url =
            case indexes of
                [] ->
                    url

                x :: xs ->
                    let
                        page_range =
                            String.append (String.fromInt (page_number * 50)) "&"
                    in
                    String.append (String.append (String.append (String.slice 0 x url) "s=") page_range) (String.slice x length url)
    in
    new_url


getVerse url =
    Http.get { url = String.append "https://corsproxy.io/?" url, expect = Http.expectString GotVerse }


getSearchVerses : String -> Int -> Cmd Msg
getSearchVerses url page_number =
    Http.get
        { url = String.append "https://corsproxy.io/?" (insert_page_number url page_number)
        , expect = Http.expectString GotSearchVerses
        }


getVerses : String -> Int -> Cmd Msg
getVerses url page_number =
    Http.get
        { url = String.append "https://corsproxy.io/?" (insert_page_number url page_number)
        , expect = Http.expectString GotVerses
        }


type alias Model =
    { rubrics : List ( String, String )
    , verses : List ( String, String )
    , page_number : Int
    , current_rubric : Int
    , lines : List String
    , all_verses : List ( String, String )
    , search_string : String
    , level : Int
    , title : String
    }


textHtml : String -> List (Html.Html msg)
textHtml t =
    case Html.Parser.run t of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes

        Err _ ->
            []


initialModel =
    { rubrics = []
    , verses = []
    , page_number = 0
    , current_rubric = 0
    , lines = []
    , all_verses = []
    , search_string = ""
    , level = 1
    , title = ""
    }


view model =
    div [ class "contents" ]
        [ if List.isEmpty model.lines then
            input [ placeholder "искать название", onInput Search ] []

          else
            h1 [] []
        , if not (List.isEmpty model.verses) || not (List.isEmpty model.lines) then
            button
                [ onClick
                    (if List.isEmpty model.lines then
                        Home

                     else if model.current_rubric == 0 then
                        Home

                     else
                        BackToRubric
                    )
                ]
                [ text (String.fromChar (Char.fromCode 8593)) ]

          else
            div [] []
        , h1 []
            [ text
                (if List.length model.verses == 0 then
                    "Рубрики"

                 else if List.isEmpty model.lines then
                    "Стихи"

                 else
                    ""
                )
            ]
        , ul []
            (if List.isEmpty model.verses && List.isEmpty model.lines then
                List.map (\e -> li [] [ a [ onClick (GetVerses (stringToUrl (Tuple.first e)) model.page_number) ] [ text (Tuple.second e) ] ]) model.rubrics

             else
                []
            )
        , div []
            [ ul []
                (if List.isEmpty model.lines then
                    List.map (\e -> li [ onClick (GetVerse (Tuple.second e) (stringToUrl (Tuple.first e))) ] [ text (Tuple.second e) ]) model.verses

                 else
                    []
                )
            ]
        , div []
            [ h1 []
                [ if not (List.isEmpty model.lines) then
                    text model.title

                  else
                    text ""
                ]
            , ul [] (List.map (\e -> li [] [ span [] (textHtml e) ]) model.lines)
            ]
        , div []
            [ h1 [] [ text (String.fromInt (List.length model.all_verses)) ] ]
        ]


find_verse html =
    let
        r =
            Maybe.withDefault Regex.never <| Regex.fromStringWith { multiline = True, caseInsensitive = True } "<div class=\"text\">(.*?)</div>"

        matches =
            Regex.find r (String.replace "\n" "" html)

        text =
            case matches of
                [] ->
                    [ "error1" ]

                x :: xs ->
                    case x.submatches of
                        [] ->
                            [ "error2" ]

                        y :: ys ->
                            let
                                out =
                                    Maybe.withDefault "error3" y

                                lines =
                                    String.split "<br>" out
                            in
                            lines
    in
    text


find_url_rubric url =
    let
        r =
            Maybe.withDefault Regex.never <| Regex.fromString "(\\d)#\\d"

        matches =
            Regex.find r url

        rubric =
            case matches of
                [] ->
                    1

                x :: xs ->
                    case x.submatches of
                        [] ->
                            1

                        y :: ys ->
                            Maybe.withDefault 1 (String.toInt (Maybe.withDefault "1" y))
    in
    rubric


stringToUrl s =
    case String.toInt s of
        Just n ->
            String.append "https://stihi.ru/avtor/smikhno&book=" (String.append (String.append s "#") s)

        Nothing ->
            String.append "https://stihi.ru" s


extractSubMatch : List (Maybe String) -> ( String, String )
extractSubMatch m =
    if List.length m == 2 then
        let
            lst =
                List.map (\e -> Maybe.withDefault "" <| e) m
        in
        ( Maybe.withDefault "" (List.head lst), Maybe.withDefault "" (List.head (Maybe.withDefault [ "" ] (List.tail lst))) )

    else
        ( "", "" )


findVerses html =
    let
        r =
            Maybe.withDefault Regex.never <| Regex.fromString "href=\"(.*)\".*class=\"poemlink\">(.*)</a>"

        matches =
            Regex.find r html
    in
    List.map (\m -> extractSubMatch m.submatches) matches


findRubrics html =
    let
        r =
            Maybe.withDefault Regex.never <| Regex.fromString "<div.*bookheader\">.*\\w{3,4}=\"(.*)\">(.*)</a>.*</div>"

        matches =
            Regex.find r html
    in
    List.map (\m -> extractSubMatch m.submatches) matches


rubric_to_url id =
    String.append (String.append (String.append "https://stihi.ru/avtor/smikhno&book=" (String.fromInt id)) "#") (String.fromInt id)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Home ->
            ( { model | level = 1 }, initialCmd )

        BackToRubric ->
            ( { model | lines = [], verses = [], level = 2 }, getVerses (rubric_to_url model.current_rubric) 0 )

        GetVerse title url ->
            ( { model | title = title }, getVerse url )

        GetVerses url page_number ->
            let
                rubric =
                    find_url_rubric url
            in
            ( { model | current_rubric = rubric }, getVerses url page_number )

        GotVerse result ->
            case result of
                Ok response ->
                    ( { model | lines = find_verse response }, Cmd.none )

                Err httpError ->
                    ( { title = "", rubrics = [ ( "error", "error" ) ], verses = [], page_number = model.page_number, current_rubric = model.current_rubric, lines = model.lines, all_verses = model.all_verses, search_string = model.search_string, level = model.level }, Cmd.none )

        GotVerses result ->
            case result of
                Ok response ->
                    let
                        html =
                            response

                        verses =
                            findVerses html

                        old_verses =
                            model.verses

                        new_verses =
                            List.append old_verses verses

                        count =
                            List.length verses

                        rubric =
                            String.append (String.append (String.fromInt model.current_rubric) "#") (String.fromInt model.current_rubric)

                        cmd =
                            if count == 50 then
                                getVerses (String.append "https://stihi.ru/avtor/smikhno&book=" rubric) (model.page_number + 1)

                            else
                                Cmd.none
                    in
                    ( { model | verses = new_verses, page_number = model.page_number + 1, current_rubric = model.current_rubric, lines = model.lines, all_verses = model.all_verses, level = 2 }, cmd )

                Err httpError ->
                    ( { title = "", rubrics = [ ( "error", "error" ) ], verses = [], page_number = model.page_number, current_rubric = model.current_rubric, lines = model.lines, all_verses = model.all_verses, search_string = model.search_string, level = model.level }, Cmd.none )

        GotData result ->
            case result of
                Ok response ->
                    let
                        html =
                            response

                        rubrics =
                            findRubrics html
                    in
                    ( { model | rubrics = rubrics, verses = [], page_number = 0, current_rubric = 0, lines = [], all_verses = model.all_verses }, Cmd.none )

                Err httpError ->
                    ( model, Cmd.none )

        GotRubrics result ->
            case result of
                Ok response ->
                    let
                        rubrics =
                            findRubrics response

                        cmd =
                            getSearchVerses "https://stihi.ru/avtor/smikhno&book=1#1" 0
                    in
                    ( { model | rubrics = rubrics, verses = [], page_number = 0, current_rubric = 1, lines = [], all_verses = model.all_verses, level = 1 }, cmd )

                Err httpError ->
                    ( model, Cmd.none )

        Search text ->
            if String.length text > 2 then
                let
                    filtered_verses =
                        List.filter (\t -> String.contains (String.toLower text) (String.toLower (Tuple.second t))) model.all_verses

                    saved_verses =
                        model.verses

                    _ =
                        Debug.log "rubrics = " model.rubrics

                    _ =
                        Debug.log "verses = " model.verses

                    saved_lines =
                        model.lines
                in
                ( { model | search_string = text, verses = filtered_verses, lines = [] }, Cmd.none )

            else if model.level == 1 then
                ( model, initialCmd )

            else
                let
                    _ =
                        Debug.log "current_rubric = " model.current_rubric

                    rubric =
                        String.append (String.append (String.fromInt model.current_rubric) "#") (String.fromInt model.current_rubric)
                in
                if model.current_rubric == 0 then
                    ( { model | verses = [] }, initialCmd )

                else
                    ( { model | verses = [] }, getVerses (String.append "https://stihi.ru/avtor/smikhno&book=" rubric) 0 )

        GotSearchVerses result ->
            case result of
                Ok response ->
                    let
                        html =
                            response

                        verses =
                            findVerses html

                        old_verses =
                            model.verses

                        new_verses =
                            List.append old_verses verses

                        count =
                            List.length verses

                        rubric =
                            String.append (String.append (String.fromInt model.current_rubric) "#") (String.fromInt model.current_rubric)

                        next_rubric =
                            String.append (String.append (String.fromInt (model.current_rubric + 1)) "#") (String.fromInt (model.current_rubric + 1))

                        page_number =
                            if count == 50 then
                                model.page_number + 1

                            else if model.current_rubric < List.length model.rubrics then
                                0

                            else
                                0

                        next_rubric_number =
                            if count == 50 then
                                model.current_rubric

                            else if model.current_rubric < List.length model.rubrics then
                                model.current_rubric + 1

                            else
                                1

                        cmd =
                            if count == 50 then
                                getSearchVerses (String.append "https://stihi.ru/avtor/smikhno&book=" rubric) page_number

                            else if model.current_rubric < List.length model.rubrics then
                                getSearchVerses (String.append "https://stihi.ru/avtor/smikhno&book=" next_rubric) 0

                            else
                                initialCmd
                    in
                    ( { title = "", rubrics = model.rubrics, verses = [], page_number = page_number, current_rubric = next_rubric_number, lines = model.lines, all_verses = List.append model.all_verses verses, search_string = model.search_string, level = model.level }, cmd )

                Err httpError ->
                    ( { title = "", rubrics = [ ( "error", "error" ) ], verses = [], page_number = model.page_number, current_rubric = model.current_rubric, lines = model.lines, all_verses = model.all_verses, search_string = model.search_string, level = model.level }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, initialSearchCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
