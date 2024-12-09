# Elm 3D Playground

Going for an `elm-playground`-like experience, but based on `elm-3d-scene`.

## Usage

Here's a really bad example I'm using with my junior-high students:

```elm
module Main exposing (main)

import Playground3D
    exposing
        ( Color
        , Computer
        , Shape
        , black
        , blue
        , sphere
        , game
        , green
        , group
        , lightGray
        , scene
        , Scene
        , move
        , rotate
        , box
        , toX
        , toY
        , white
        , withCamera
        , yellow
        , scene
        , withLighting
        , Lighting(..)
        )
import Random
import Direction3d
import Angle
import Camera3d
import Viewpoint3d
import Point3d
import Length

type alias Position =
    { x : Float
    , y : Float
    , z : Float
    }


type alias Size =
    { width : Float
    , height : Float
    , depth : Float
    }


type alias Rect =
    { position : Position
    , size : Size
    }


type alias Model =
    { player : Position
    , stars : List Star
    , score : Int
    , clock : Int
    }


type alias Star =
    { position : Position
    , collected : Bool
    }


screenMax =
    200


screenMin =
    -200


initialModel : Model
initialModel =
    { player = { x = -100, y = -100, z = -500 }
    , score = 0
    , clock = 0
    , stars = makeStar 20 9450954609
    }


makeStar : Int -> Int -> List Star
makeStar count random =
    let
        x =
            getRandom screenMin screenMax random

        y =
            getRandom screenMin screenMax x

        z =
            getRandom screenMin screenMax y

        position =
            { x = toFloat x, y = toFloat y, z = toFloat z }

        star =
            { collected = False, position = position }
    in
    if count > 0 then
        star :: makeStar (count - 1) y

    else
        [ star ]


main =
    game view update initialModel


isShowing star =
    star.collected == False


view : Computer -> Model -> Scene coordinates
view computer model =
    let
        playerRect =
            { position = model.player, size = { width = 40, height = 40 } }

        stars =
            model.stars |> List.filter isShowing

        size =
            20 + toFloat model.score * 10
    in
    ((stars |> List.map drawEachStar)
        ++ [ sphere black size |> move model.player.x model.player.y model.player.z
           ])
        |> scene
        |> withLighting (Sunny (Direction3d.yz (Angle.degrees -120)) False)
        |> withCamera
            (Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.centimeters 40 20 40
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 60
                }
            )


drawEachStar : Star -> Shape
drawEachStar star =
    drawStar
        |> move
            star.position.x
            star.position.y
            star.position.z

toCube : Position -> Float -> Rect
toCube position size =
    { position = position
    , size = { width = size, height = size, depth = size }
    }


checkStar : Int -> Position -> Star -> Star
checkStar score player star =
    { star
        | collected =
            star.collected
                || areOverlapping
                    (toCube player (30 + toFloat score * 10))
                    (toCube star.position 20)
    }


moveStar star =
    let
        newX =
            star.position.x - 1

        newY =
            star.position.y - 1

        newZ =
            star.position.z - 1
    in
    { star
        | position =
            { x = newX
            , y = newY
            , z = newZ
            }
    }


update : Computer -> Model -> Model
update computer ({ player, stars } as model) =
    let
        position =
            { x = player.x + toX computer.keyboard * 2
            , y = player.y + toY computer.keyboard * 2
            , z = player.z
            }

        moreStars =
            if (stars |> List.length) < 20 then
                makeStar 5 model.clock

            else
                []

        newStars =
            stars
                |> List.map (checkStar model.score position)
                |> List.map moveStar

        newScore =
            newStars |> List.filter isCollected |> List.length
    in
    { model
        | player = position
        , stars =
            newStars
                |> List.filter (not << isCollected)
                |> List.filter (not << isOffScreen)
        , score = newScore + model.score
        , clock = model.clock + 1
    }


isOffScreen star =
    star.position.x < screenMin || star.position.y < screenMin


isCollected star =
    star.collected


drawStar : Shape
drawStar =
    group
        [ sphere yellow 20
        ]


getRandom : Int -> Int -> Int -> Int
getRandom min max previous =
    previous
        |> Random.initialSeed
        |> Random.step (Random.int min max)
        |> Tuple.first


areOverlapping : Rect -> Rect -> Bool
areOverlapping rect1 rect2 =
    let
        left1 =
            rect1.position.x

        right1 =
            rect1.position.x + rect1.size.width

        top1 =
            rect1.position.y

        bottom1 =
            rect1.position.y + rect1.size.height

        left2 =
            rect2.position.x

        right2 =
            rect2.position.x + rect2.size.width

        top2 =
            rect2.position.y

        bottom2 =
            rect2.position.y + rect2.size.height
    in
    not (left1 >= right2 || right1 <= left2 || top1 >= bottom2 || bottom1 <= top2)
```