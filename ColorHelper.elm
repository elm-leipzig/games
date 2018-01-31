module ColorHelper exposing (colorNr, grey, white)

import Array exposing (Array)
import Char
import Color exposing (Color)
import Colorbrewer.Diverging exposing (spectral11)


white : String
white =
    "#FFFFFF"


grey : String
grey =
    "#AAAAAA"


colors : Array String
colors =
    spectral11 |> List.map (\( r, g, b ) -> colorToHex (Color.rgb r g b)) |> Array.fromList


colorNr : Int -> String
colorNr n =
    Array.get n colors |> Maybe.withDefault "#aaa"


colorToHex : Color -> String
colorToHex cl =
    let
        { red, green, blue } =
            Color.toRgb cl
    in
    List.map toHex [ red, green, blue ]
        |> (::) "#"
        |> String.join ""


toHex : Int -> String
toHex =
    toRadix >> String.padLeft 2 '0'


toRadix : Int -> String
toRadix n =
    let
        getChr c =
            if c < 10 then
                toString c
            else
                String.fromChar <| Char.fromCode (87 + c)
    in
    if n < 16 then
        getChr n
    else
        toRadix (n // 16) ++ getChr (n % 16)
