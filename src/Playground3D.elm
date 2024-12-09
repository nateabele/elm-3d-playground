module Playground3D exposing
    ( withCamera
    , withLighting
    , withShadow
    , Scene
    , Shape
    , yellow
    , black
    , blue
    , box
    , brown
    , charcoal
    , darkBlue
    , darkBrown
    , darkCharcoal
    , darkGray
    , darkGreen
    , darkGrey
    , darkOrange
    , darkPurple
    , darkRed
    , darkYellow
    , fromColor
    , group
    , lightBlue
    , lightBrown
    , lightCharcoal
    , lightGray
    , lightGreen
    , lightGrey
    , lightOrange
    , lightPurple
    , lightRed
    , lightYellow
    , Lighting(..)
    , move
    , moveBack
    , moveDown
    , moveForward
    , moveLeft
    , moveRight
    , moveUp
    , moveX
    , moveY
    , moveZ
    , orange
    , purple
    , red
    , rgb
    , rotate
    , grey
    , gray
    , green
    , game
    , scale
    , scene
    , white
    , Computer
    , Keyboard
    , Mouse
    , Screen
    , Time
    , Number
    , sphere
    , toX
    , toXY
    , toY
    )

{-| Contains most of the usual suspects from elm-playground, just 3D-ified.

@docs withCamera
@docs withLighting
@docs withShadow
@docs Scene
@docs Shape
@docs yellow
@docs black
@docs blue
@docs box
@docs brown
@docs charcoal
@docs darkBlue
@docs darkBrown
@docs darkCharcoal
@docs darkGray
@docs darkGreen
@docs darkGrey
@docs darkOrange
@docs darkPurple
@docs darkRed
@docs darkYellow
@docs fromColor
@docs group
@docs lightBlue
@docs lightBrown
@docs lightCharcoal
@docs lightGray
@docs lightGreen
@docs lightGrey
@docs lightOrange
@docs lightPurple
@docs lightRed
@docs lightYellow
@docs Lighting
@docs move
@docs moveBack
@docs moveDown
@docs moveForward
@docs moveLeft
@docs moveRight
@docs moveUp
@docs moveX
@docs moveY
@docs moveZ
@docs orange
@docs purple
@docs red
@docs rgb
@docs rotate
@docs grey
@docs gray
@docs green
@docs game
@docs scale
@docs scene
@docs white
@docs Computer
@docs Keyboard
@docs Mouse
@docs Screen
@docs Time
@docs Number
@docs sphere
@docs toX
@docs toXY
@docs toY

-}

import Angle
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (Direction3d)
import Frame3d
import Html
import Json.Decode as D
import Length exposing (Length)
import Pixels
import Point3d
import Quantity exposing (Quantity(..))
import Scene3d
import Scene3d.Material as Material exposing (Material)
import Set
import Sphere3d
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time
import Viewpoint3d


type alias Color =
    Color.Color



-- COMPUTER


{-| When writing a [`game`](#game), you can look up all sorts of information
about your computer:

  - [`Mouse`](#Mouse) - Where is the mouse right now?
  - [`Keyboard`](#Keyboard) - Are the arrow keys down?
  - [`Screen`](#Screen) - How wide is the screen?
  - [`Time`](#Time) - What time is it right now?

So you can use expressions like `computer.mouse.x` and `computer.keyboard.enter`
in games where you want some mouse or keyboard interaction.

-}
type alias Computer =
    { mouse : Mouse
    , keyboard : Keyboard
    , screen : Screen
    , time : Time
    }



-- type alias Sceneription coordinates =
--     { camera : Camera3d Length.Meters coordinates
--     , clipDepth : Length
--     , dimensions : ( Int, Int )
--     , background : Scene3d.Background coordinates
--     , entities : List Shape
--     }


{-| Create a material from a color, that can be applied to a shape
-}
fromColor : Color -> Material coordinates { attributes | normals : () }
fromColor color =
    Material.nonmetal
        { baseColor = color
        , roughness = 0.4
        }



-- MOUSE


{-| Information about the mouse
-}
type alias Mouse =
    { x : Number
    , y : Number
    , down : Bool
    , click : Bool
    }


{-| A number like `1` or `3.14` or `-120`.
-}
type alias Number =
    Float



-- KEYBOARD


{-| Figure out what is going on with the keyboard.

If someone is pressing the UP and RIGHT arrows, you will see a value like this:

    { up = True
    , down = False
    , left = False
    , right = True
    , space = False
    , enter = False
    , shift = False
    , backspace = False
    , keys = Set.fromList [ "ArrowUp", "ArrowRight" ]
    }

So if you want to move a character based on arrows, you could write an update
like this:

    update computer y =
        if computer.keyboard.up then
            y + 1

        else
            y

Check out [`toX`](#toX) and [`toY`](#toY) which make this even easier!

**Note:** The `keys` set will be filled with the name of all keys which are
down right now. So you will see things like `"a"`, `"b"`, `"c"`, `"1"`, `"2"`,
`"Space"`, and `"Control"` in there. Check out [this list][list] to see the
names used for all the different special keys! From there, you can use
[`Set.member`][member] to check for whichever key you want. E.g.
`Set.member "Control" computer.keyboard.keys`.

[list]: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values
[member]: /packages/elm/core/latest/Set#member

-}
type alias Keyboard =
    { up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    , space : Bool
    , enter : Bool
    , shift : Bool
    , backspace : Bool
    , keys : Set.Set String
    }


{-| Turn the LEFT and RIGHT arrows into a number.

    toX { left = False, right = False, ... } == 0
    toX { left = True , right = False, ... } == -1
    toX { left = False, right = True , ... } == 1
    toX { left = True , right = True , ... } == 0

So to make a square move left and right based on the arrow keys, we could say:

    import Playground exposing (..)

    main =
        game view update 0

    view computer x =
        [ square green 40
            |> moveX x
        ]

    update computer x =
        x + toX computer.keyboard

-}
toX : Keyboard -> Number
toX keyboard =
    ifElse keyboard.right 1 0 - ifElse keyboard.left 1 0


{-| Turn the UP and DOWN arrows into a number.

    toY { up = False, down = False, ... } == 0
    toY { up = True , down = False, ... } == 1
    toY { up = False, down = True , ... } == -1
    toY { up = True , down = True , ... } == 0

This can be used to move characters around in games just like [`toX`](#toX):

    import Playground exposing (..)

    main =
        game view update ( 0, 0 )

    view computer ( x, y ) =
        [ square blue 40
            |> move x y
        ]

    update computer ( x, y ) =
        ( x + toX computer.keyboard
        , y + toY computer.keyboard
        )

-}
toY : Keyboard -> Number
toY keyboard =
    ifElse keyboard.up 1 0 - ifElse keyboard.down 1 0


{-| If you just use `toX` and `toY`, you will move diagonal too fast. You will go
right at 1 pixel per update, but you will go up/right at 1.41421 pixels per
update.

So `toXY` turns the arrow keys into an `(x,y)` pair such that the distance is
normalized:

    toXY { up = True , down = False, left = False, right = False, ... } == (1, 0)
    toXY { up = True , down = False, left = False, right = True , ... } == (0.707, 0.707)
    toXY { up = False, down = False, left = False, right = True , ... } == (0, 1)

Now when you go up/right, you are still going 1 pixel per update.

    import Playground exposing (..)

    main =
        game view update ( 0, 0 )

    view computer ( x, y ) =
        [ square green 40
            |> move x y
        ]

    update computer ( x, y ) =
        let
            ( dx, dy ) =
                toXY computer.keyboard
        in
        ( x + dx, y + dy )

-}
toXY : Keyboard -> ( Number, Number )
toXY keyboard =
    let
        x =
            toX keyboard

        y =
            toY keyboard
    in
    if x /= 0 && y /= 0 then
        ( x / squareRootOfTwo, y / squareRootOfTwo )

    else
        ( x, y )


squareRootOfTwo : Number
squareRootOfTwo =
    sqrt 2



-- SCREEN


{-| Get the dimensions of the screen. If the screen is 800 by 600, you will see
a value like this:

    { width = 800
    , height = 600
    , top = 300
    , left = -400
    , right = 400
    , bottom = -300
    }

This can be nice when used with [`moveY`](#moveY) if you want to put something
on the bottom of the screen, no matter the dimensions.

-}
type alias Screen =
    { width : Number
    , height : Number
    , top : Number
    , left : Number
    , right : Number
    , bottom : Number
    }



-- TIME


{-| The current time.

Helpful when making an [`animation`](#animation) with functions like
[`spin`](#spin), [`wave`](#wave), and [`zigzag`](#zigzag).

-}
type alias Time =
    Time.Posix



-- GAME


{-| Create a game!

Once you get comfortable with [`animation`](#animation), you can try making a
game with the keyboard and mouse. Here is an example of a green square that
just moves to the right:

    import Playground exposing (..)

    main =
        game view update 0

    view computer offset =
        [ square green 40
            |> moveRight offset
        ]

    update computer offset =
        offset + 0.03

This shows the three important parts of a game:

1.  `memory` - makes it possible to store information. So with our green square,
    we save the `offset` in memory. It starts out at `0`.
2.  `view` - lets us say which shapes to put on screen. So here we move our
    square right by the `offset` saved in memory.
3.  `update` - lets us update the memory. We are incrementing the `offset` by
    a tiny amount on each frame.

The `update` function is called about 60 times per second, so our little
changes to `offset` start to add up pretty quickly!

This game is not very fun though! Making a `game` also gives you access to the
[`Computer`](#Computer), so you can use information about the [`Mouse`](#Mouse)
and [`Keyboard`](#Keyboard) to make it interactive! So here is a red square that
moves based on the arrow keys:

    import Playground exposing (..)

    main =
        game view update ( 0, 0 )

    view computer ( x, y ) =
        [ square red 40
            |> move x y
        ]

    update computer ( x, y ) =
        ( x + toX computer.keyboard
        , y + toY computer.keyboard
        )

Notice that in the `update` we use information from the keyboard to update the
`x` and `y` values. These building blocks let you make pretty fancy games!

-}
game :
    (Computer -> memory -> Scene coordinates)
    -> (Computer -> memory -> memory)
    -> memory
    -> Program () (Game memory) Msg
game viewMemory updateMemory initialMemory =
    let
        init () =
            ( Game E.Visible initialMemory initialComputer
            , Task.perform GotViewport Dom.getViewport
            )

        view (Game _ memory computer) =
            { title = "Playground"
            , body = [ render computer.screen (viewMemory computer memory) ]
            }

        update msg model =
            ( gameUpdate updateMemory msg model
            , Cmd.none
            )

        subscriptions (Game visibility _ _) =
            case visibility of
                E.Hidden ->
                    E.onVisibilityChange VisibilityChanged

                E.Visible ->
                    gameSubscriptions
    in
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initialComputer : Computer
initialComputer =
    { mouse = Mouse 0 0 False False
    , keyboard = emptyKeyboard
    , screen = toScreen 600 600
    , time = Time.millisToPosix 0
    }



-- SUBSCRIPTIONS


gameSubscriptions : Sub Msg
gameSubscriptions =
    Sub.batch
        [ E.onResize Resized
        , E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
        , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
        , E.onAnimationFrame Tick
        , E.onVisibilityChange VisibilityChanged
        , E.onClick (D.succeed MouseClick)
        , E.onMouseDown (D.succeed (MouseButton True))
        , E.onMouseUp (D.succeed (MouseButton False))
        , E.onMouseMove (D.map2 MouseMove (D.field "pageX" D.float) (D.field "pageY" D.float))
        ]



-- GAME HELPERS


type Game memory
    = Game E.Visibility memory Computer


type Msg
    = KeyChanged Bool String
    | Tick Time.Posix
    | GotViewport Dom.Viewport
    | Resized Int Int
    | VisibilityChanged E.Visibility
    | MouseMove Float Float
    | MouseClick
    | MouseButton Bool


gameUpdate : (Computer -> memory -> memory) -> Msg -> Game memory -> Game memory
gameUpdate updateMemory msg (Game vis memory computer) =
    case msg of
        Tick time ->
            Game vis (updateMemory computer memory) <|
                if computer.mouse.click then
                    { computer | time = time, mouse = mouseClick False computer.mouse }

                else
                    { computer | time = time }

        GotViewport { viewport } ->
            Game vis memory { computer | screen = toScreen viewport.width viewport.height }

        Resized w h ->
            Game vis memory { computer | screen = toScreen (toFloat w) (toFloat h) }

        KeyChanged isDown key ->
            Game vis memory { computer | keyboard = updateKeyboard isDown key computer.keyboard }

        MouseMove pageX pageY ->
            let
                x =
                    computer.screen.left + pageX

                y =
                    computer.screen.top - pageY
            in
            Game vis memory { computer | mouse = mouseMove x y computer.mouse }

        MouseClick ->
            Game vis memory { computer | mouse = mouseClick True computer.mouse }

        MouseButton isDown ->
            Game vis memory { computer | mouse = mouseDown isDown computer.mouse }

        VisibilityChanged visibility ->
            Game visibility
                memory
                { computer
                    | keyboard = emptyKeyboard
                    , mouse = Mouse computer.mouse.x computer.mouse.y False False
                }



-- SCREEN HELPERS


toScreen : Float -> Float -> Screen
toScreen width height =
    { width = width
    , height = height
    , top = height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = -height / 2
    }



-- MOUSE HELPERS


mouseClick : Bool -> Mouse -> Mouse
mouseClick bool mouse =
    { mouse | click = bool }


mouseDown : Bool -> Mouse -> Mouse
mouseDown bool mouse =
    { mouse | down = bool }


mouseMove : Float -> Float -> Mouse -> Mouse
mouseMove x y mouse =
    { mouse | x = x, y = y }



-- KEYBOARD HELPERS


emptyKeyboard : Keyboard
emptyKeyboard =
    { up = False
    , down = False
    , left = False
    , right = False
    , space = False
    , enter = False
    , shift = False
    , backspace = False
    , keys = Set.empty
    }


updateKeyboard : Bool -> String -> Keyboard -> Keyboard
updateKeyboard isDown key keyboard =
    let
        keys =
            if isDown then
                Set.insert key keyboard.keys

            else
                Set.remove key keyboard.keys
    in
    case key of
        " " ->
            { keyboard | keys = keys, space = isDown }

        "Enter" ->
            { keyboard | keys = keys, enter = isDown }

        "Shift" ->
            { keyboard | keys = keys, shift = isDown }

        "Backspace" ->
            { keyboard | keys = keys, backspace = isDown }

        "ArrowUp" ->
            { keyboard | keys = keys, up = isDown }

        "ArrowDown" ->
            { keyboard | keys = keys, down = isDown }

        "ArrowLeft" ->
            { keyboard | keys = keys, left = isDown }

        "ArrowRight" ->
            { keyboard | keys = keys, right = isDown }

        _ ->
            { keyboard | keys = keys }



-- SHAPES


{-| Shapes help you make a `picture`, `animation`, or `game`.

Read on to see examples of [`circle`](#circle), [`rectangle`](#rectangle),
[`words`](#words), [`image`](#image), and many more!

-}
type Shape
    = Shape
        ( Number, Number, Number )
        -- x, y, z
        -- angle
        Number
        -- scale
        Number
        Bool
        -- Shadow
        Form


type Form
    = Sphere Color Number
    | Box Color Number Number Number
      -- | Cylinder Color Number Number
      -- | Cone Color Number Number
      -- | Polygon Color (List ( Number, Number ))
      -- | Words Color String
    | Group (List Shape)


{-| Create a sphere
-}
sphere : Color -> Number -> Shape
sphere color radius =
    Shape ( 0, 0, 0 ) 0 1 False (Sphere color radius)


{-| Create a box, like a cube or other shape
-}
box : Color -> Number -> Number -> Number -> Shape
box color width height depth =
    Shape ( 0, 0, 0 ) 0 1 False (Box color width height depth)


{-| Group shapes together.

    import Playground3D exposing (..)

    main =
        picture
            [ group [ sphere red 10, sphere blue 10 ] ]

-}
group : List Shape -> Shape
group shapes =
    Shape ( 0, 0, 0 ) 0 1 False (Group shapes)


{-| Add a shadow to a shape.
-}
withShadow : Bool -> Shape -> Shape
withShadow shadow (Shape coords a s _ f) =
    Shape coords a s shadow f



-- TRANSFORMS


{-| Move a shape in one or more directions
-}
move : Number -> Number -> Number -> Shape -> Shape
move dx dy dz (Shape ( x, y, z ) a s o f) =
    Shape ( x + dx, y + dy, z + dz ) a s o f


{-| Move a shape up
-}
moveUp : Number -> Shape -> Shape
moveUp =
    moveY


{-| Move a shape down
-}
moveDown : Number -> Shape -> Shape
moveDown dy (Shape ( x, y, z ) a s o f) =
    Shape ( x, y - dy, z ) a s o f


{-| Move a shape left
-}
moveLeft : Number -> Shape -> Shape
moveLeft dx (Shape ( x, y, z ) a s o f) =
    Shape ( x - dx, y, z ) a s o f


{-| Move a shape right
-}
moveRight : Number -> Shape -> Shape
moveRight =
    moveX


{-| Move a shape forward
-}
moveForward : Number -> Shape -> Shape
moveForward =
    moveZ


{-| Move a shape back
-}
moveBack : Number -> Shape -> Shape
moveBack dz (Shape ( x, y, z ) a s o f) =
    Shape ( x, y, z - dz ) a s o f


{-| Move a shape left or right
-}
moveX : Number -> Shape -> Shape
moveX dx (Shape ( x, y, z ) a s o f) =
    Shape ( x + dx, y, z ) a s o f


{-| Move a shape up or down
-}
moveY : Number -> Shape -> Shape
moveY dy (Shape ( x, y, z ) a s o f) =
    Shape ( x, y + dy, z ) a s o f


{-| Move a shape forward or backward
-}
moveZ : Number -> Shape -> Shape
moveZ dz (Shape ( x, y, z ) a s o f) =
    Shape ( x, y, z + dz ) a s o f


{-| Scale a shape
-}
scale : Number -> Shape -> Shape
scale ns (Shape coords a s o f) =
    Shape coords a (s * ns) o f


{-| Rotate shapes in degrees.

    import Playground exposing (..)

    main =
        picture
            [ words black "These words are tilted!"
                |> rotate 10
            ]

The degrees go **counter-clockwise** to match the direction of the
[unit circle](https://en.wikipedia.org/wiki/Unit_circle).

-}
rotate : Number -> Shape -> Shape
rotate da (Shape coords a s o f) =
    Shape coords (a + da) s o f



-- COLOR


{-| The color light yellow
-}
lightYellow : Color
lightYellow =
    Color.lightYellow


{-| The color yellow
-}
yellow : Color
yellow =
    Color.yellow


{-| The color dark yellow
-}
darkYellow : Color
darkYellow =
    Color.darkYellow


{-| The color light orange
-}
lightOrange : Color
lightOrange =
    Color.lightOrange


{-| The color orange
-}
orange : Color
orange =
    Color.orange


{-| The color dark orange
-}
darkOrange : Color
darkOrange =
    Color.darkOrange


{-| The color light brown
-}
lightBrown : Color
lightBrown =
    Color.lightBrown


{-| The color brown
-}
brown : Color
brown =
    Color.brown


{-| The color dark brown
-}
darkBrown : Color
darkBrown =
    Color.darkBrown


{-| The color light green
-}
lightGreen : Color
lightGreen =
    Color.lightGreen


{-| The color green
-}
green : Color
green =
    Color.green


{-| The color dark green
-}
darkGreen : Color
darkGreen =
    Color.darkGreen


{-| The color light blue
-}
lightBlue : Color
lightBlue =
    Color.lightBlue


{-| The color blue
-}
blue : Color
blue =
    Color.blue


{-| The color dark blue
-}
darkBlue : Color
darkBlue =
    Color.darkBlue


{-| The color light purple
-}
lightPurple : Color
lightPurple =
    Color.lightPurple


{-| The color purple
-}
purple : Color
purple =
    Color.purple


{-| The color dark purple
-}
darkPurple : Color
darkPurple =
    Color.darkPurple


{-| The color light red
-}
lightRed : Color
lightRed =
    Color.lightRed


{-| The color red
-}
red : Color
red =
    Color.red


{-| The color dark red
-}
darkRed : Color
darkRed =
    Color.darkRed


{-| The color light grey
-}
lightGrey : Color
lightGrey =
    Color.lightGray


{-| The color grey
-}
grey : Color
grey =
    Color.gray


{-| The color dark grey
-}
darkGrey : Color
darkGrey =
    Color.darkGray


{-| The color light charcoal
-}
lightCharcoal : Color
lightCharcoal =
    Color.darkGray


{-| The color charcoal
-}
charcoal : Color
charcoal =
    Color.darkGray


{-| The color darkCharcoal
-}
darkCharcoal : Color
darkCharcoal =
    Color.darkGray


{-| The color white
-}
white : Color
white =
    Color.white


{-| The color black
-}
black : Color
black =
    Color.black



-- ALTERNATE SPELLING GREYS


{-| -}
lightGray : Color
lightGray =
    Color.lightGray


{-| -}
gray : Color
gray =
    Color.gray


{-| -}
darkGray : Color
darkGray =
    Color.darkGray



-- CUSTOM COLORS


{-| RGB stands for Red-Green-Blue. With these three parts, you can create any
color you want. For example:

    brightBlue =
        rgb 18 147 216

    brightGreen =
        rgb 119 244 8

    brightPurple =
        rgb 94 28 221

Each number needs to be between 0 and 255.

It can be hard to figure out what numbers to pick, so try using a color picker
like [paletton] to find colors that look nice together. Once you find nice
colors, click on the color previews to get their RGB values.

[paletton]: http://paletton.com/

-}
rgb : Number -> Number -> Number -> Color
rgb r g b =
    Color.rgb (toFloat (colorClamp r)) (toFloat (colorClamp g)) (toFloat (colorClamp b))


colorClamp : Number -> Int
colorClamp number =
    clamp 0 255 (round number)



-- RENDER


{-| A scene is a collection of shapes, camera, lighting, and background
-}
type alias Scene coordinates =
    { camera : Camera3d Length.Meters coordinates
    , clipDepth : Length
    , background : Scene3d.Background coordinates
    , shapes : List Shape
    , lighting : Lighting coordinates
    }


type alias CastShadows =
    Bool


{-| Different types of lighting that the scene can have
-}
type Lighting coordinates
    = Unlit
    | Sunny (Direction3d coordinates) CastShadows


{-| Create a scene by giving it some shapes. Using other functions, you can set the camera and lighting.
-}
scene : List Shape -> Scene coordinates
scene shapes =
    { camera =
        Camera3d.perspective
            { viewpoint =
                Viewpoint3d.lookAt
                    { focalPoint = Point3d.origin
                    , eyePoint = Point3d.centimeters 40 20 40
                    , upDirection = Direction3d.positiveZ
                    }
            , verticalFieldOfView = Angle.degrees 60
            }
    , clipDepth = Length.meters 0.1
    , background = Scene3d.transparentBackground
    , shapes = shapes
    , lighting = Unlit
    }


{-| Configure the camera for the scene
-}
withCamera : Camera3d Length.Meters coordinates -> Scene coordinates -> Scene coordinates
withCamera camera s =
    { s | camera = camera }


{-| Set the lighting for the scene
-}
withLighting : Lighting coordinates -> Scene coordinates -> Scene coordinates
withLighting lighting s =
    { s | lighting = lighting }


render : Screen -> Scene coordinates -> Html.Html msg
render screen { camera, clipDepth, background, shapes, lighting } =
    let
        dimensions =
            ( Pixels.int (round screen.width), Pixels.int (round screen.height) )
    in
    case lighting of
        Unlit ->
            Scene3d.unlit
                { camera = camera
                , clipDepth = clipDepth
                , dimensions = dimensions
                , background = background
                , entities = shapes |> List.map renderShape
                }

        Sunny direction castShadows ->
            Scene3d.sunny
                { camera = camera
                , clipDepth = clipDepth
                , dimensions = dimensions
                , background = background
                , shadows = castShadows
                , sunlightDirection = direction
                , entities = shapes |> List.map renderShape
                , upDirection = Direction3d.positiveZ
                }



-- TODO try adding Svg.Lazy to renderShape
--
-- renderShape : Shape -> Svg msg


renderShape : Shape -> Scene3d.Entity coordinates
renderShape (Shape ( x, y, z ) angle s shadowed form) =
    case form of
        Sphere color radius ->
            renderSphere color radius x y z angle s

        Box color width height depth ->
            renderBox color width height depth x y z angle s

        Group shapes ->
            shapes
                |> List.map renderShape
                |> Scene3d.group



-- RENDER CIRCLE AND OVAL


renderSphere : Color -> Number -> Number -> Number -> Number -> Number -> Number -> Scene3d.Entity coordinates
renderSphere color radius x y z _ s =
    Scene3d.sphere (fromColor color) <|
        Sphere3d.withRadius (Length.centimeters radius) (Point3d.centimeters x y z)



-- RENDER RECTANGLE AND IMAGE


renderBox : Color -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Scene3d.Entity coordinates
renderBox color width height depth x y z angle s =
    let
        dimensions =
            ( Length.centimeters width
            , Length.centimeters height
            , Length.centimeters depth
            )
    in
    Scene3d.block (fromColor color)
        -- vv |> Scene3d.rotateAround (Axis3d.through (Point3d.centimeters x y z) (Vector3d.centimeters 0 0 1)) angle
        (Block3d.centeredOn (Frame3d.atPoint (Point3d.centimeters x y z)) dimensions)



-- UTIL


ifElse : Bool -> a -> a -> a
ifElse condition thenBranch elseBranch =
    if condition then
        thenBranch

    else
        elseBranch
