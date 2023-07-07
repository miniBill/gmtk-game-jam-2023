module Game exposing (Flags, Model, Msg, Vector2d, World, init, loadedTexture, onAnimationFrame, subscriptions, time, update, view)

import Acceleration exposing (MetersPerSecondSquared)
import Browser.Events
import Circle2d exposing (Circle2d)
import Dict exposing (Dict)
import Duration exposing (Duration)
import Ecs
import Ecs.Component as Component
import Ecs.Config as Config
import Ecs.Entity as Entity
import Ecs.System as System exposing (System)
import Effect exposing (Effect)
import Gamepad exposing (Gamepad)
import Gamepad.Simple exposing (FrameStuff)
import Html exposing (Html)
import Length exposing (Meters)
import Math.Vector2 exposing (Vec2, vec2)
import Point2d
import Quantity exposing (Quantity)
import Speed exposing (MetersPerSecond, Speed)
import Time
import Vector2d
import WebGL
import WebGL.Shape2d as Shape2d exposing (Color, rgb)
import WebGL.Shape2d.Render as Render exposing (Height, Width)
import WebGL.Shape2d.SolidShape as SolidShape exposing (SolidShape)
import WebGL.Texture as Texture exposing (Texture)
import Xbox


type alias Vector2d unit =
    Vector2d.Vector2d unit ()


type alias Point2d =
    Point2d.Point2d Meters ()


type alias Circle2d =
    Circle2d.Circle2d Meters ()


type alias Flags =
    { now : Time.Posix
    , width : Width
    , height : Height
    }


type Msg
    = Tick FrameStuff
    | Resize Int Int
    | LoadedTexture String (Result Texture.Error Texture)


type alias Model =
    { world : World
    , ball : Ecs.Entity
    , now : Time.Posix
    , width : Width
    , height : Height
    , textures : Dict String Texture
    }


type alias World =
    { ecsConfig : Ecs.Config
    , positionComponent : Ecs.Component (Vector2d Meters)
    , speedComponent : Ecs.Component (Vector2d MetersPerSecond)
    , accelerationComponent : Ecs.Component (Vector2d MetersPerSecondSquared)
    }


ecsConfigSpec : Config.Spec World
ecsConfigSpec =
    { get = .ecsConfig
    , set = \config world -> { world | ecsConfig = config }
    }


positionSpec : Component.Spec (Vector2d Meters) World
positionSpec =
    { get = .positionComponent
    , set = \positionComponent world -> { world | positionComponent = positionComponent }
    }


speedSpec : Component.Spec (Vector2d MetersPerSecond) World
speedSpec =
    { get = .speedComponent
    , set = \speedComponent world -> { world | speedComponent = speedComponent }
    }


accelerationSpec : Component.Spec (Vector2d MetersPerSecondSquared) World
accelerationSpec =
    { get = .accelerationComponent
    , set = \accelerationComponent world -> { world | accelerationComponent = accelerationComponent }
    }


applySpeeds : Duration -> System World
applySpeeds deltaT =
    System.map2
        (\( speed, _ ) ( position, setPosition ) ->
            let
                deltaX : Vector2d Meters
                deltaX =
                    Vector2d.for deltaT speed
            in
            setPosition (Vector2d.plus deltaX position)
        )
        speedSpec
        positionSpec


applyAccelerations : Duration -> System World
applyAccelerations deltaT =
    System.map2
        (\( accelleration, _ ) ( speed, setSpeed ) ->
            let
                deltaV : Vector2d MetersPerSecond
                deltaV =
                    Vector2d.for deltaT accelleration

                newSpeed : Vector2d MetersPerSecond
                newSpeed =
                    speed
                        |> Vector2d.plus deltaV

                limiter : Speed
                limiter =
                    Quantity.min
                        (Vector2d.length newSpeed)
                        maxSpeed
            in
            newSpeed
                |> Vector2d.scaleTo limiter
                |> Vector2d.scaleBy 0.9
                |> preventWiggle (Speed.metersPerSecond 0.01)
                |> setSpeed
        )
        accelerationSpec
        speedSpec


maxSpeed : Speed
maxSpeed =
    Speed.kilometersPerHour 1000


{-| Force a very small vector to zero. Avoids "wiggles".
-}
preventWiggle : Quantity Float units -> Vector2d.Vector2d units coordinates -> Vector2d.Vector2d units coordinates
preventWiggle cap vec =
    if Quantity.abs (Vector2d.length vec) |> Quantity.lessThan cap then
        Vector2d.zero

    else
        vec


{-| This is the width and height of the play area.
-}
size : number
size =
    200


minx : Float
minx =
    -size / 2


miny : Float
miny =
    -size / 2


view : Model -> Html Msg
view model =
    let
        scale : Float
        scale =
            min model.width model.height / size

        entities : List WebGL.Entity
        entities =
            [ viewBackground model
            , viewBall model
            , viewClipper model
            , viewPrompts model
            ]
                |> SolidShape.group
                |> Shape2d.scale scale scale
                |> List.singleton
                |> SolidShape.toEntities model
    in
    Shape2d.view
        { screen = model
        , entities = entities
        }


viewPrompts : Model -> SolidShape
viewPrompts model =
    let
        drawingSize : Float
        drawingSize =
            size / 20
    in
    [ viewImage Xbox.a drawingSize model
        |> Shape2d.move (minx + drawingSize) (miny + drawingSize)
    , viewImage Xbox.b drawingSize model
        |> Shape2d.move (minx + drawingSize * 2) (miny + drawingSize)
    ]
        |> SolidShape.group


viewImage : { path : String, width : Int, height : Int } -> Float -> Model -> SolidShape
viewImage image drawingSize model =
    case Dict.get image.path model.textures of
        Nothing ->
            SolidShape.group []

        Just texture ->
            let
                textureSize : Vec2
                textureSize =
                    vec2
                        (toFloat image.width)
                        (toFloat image.height)
            in
            Render.image texture textureSize
                |> SolidShape.shape drawingSize drawingSize


viewBackground : Model -> SolidShape
viewBackground _ =
    rectangle bgColor size size


bgColor : Color
bgColor =
    rgb 0 0 0


viewClipper : Model -> SolidShape
viewClipper _ =
    SolidShape.group
        [ rectangle clipColor (size * 5) (size * 5)
            |> Shape2d.move (-3 * size) 0
        , rectangle clipColor (size * 5) (size * 5)
            |> Shape2d.move (3 * size) 0
        , rectangle clipColor (size * 5) (size * 5)
            |> Shape2d.move 0 (-3 * size)
        , rectangle clipColor (size * 5) (size * 5)
            |> Shape2d.move 0 (3 * size)
        ]


clipColor : Color
clipColor =
    rgb 0 0 0


viewBall : Model -> SolidShape
viewBall ({ ball } as model) =
    let
        center : Point2d
        center =
            Component.get ball model.world.positionComponent
                |> Maybe.withDefault Vector2d.zero
                |> Vector2d.components
                |> (\( x, y ) -> Point2d.xy x y)
    in
    circle (rgb 255 0 0) (Circle2d.withRadius Length.meter center)


rectangle : Color -> Width -> Height -> SolidShape
rectangle color w h =
    Render.rect color
        |> SolidShape.shape w h


circle : Color -> Circle2d -> SolidShape
circle color circ =
    let
        r : Float
        r =
            Circle2d.radius circ
                |> Length.inMeters

        move : { x : Float, y : Float }
        move =
            Circle2d.centerPoint circ
                |> Point2d.toMeters
    in
    Render.circle color
        |> SolidShape.shape (r * 2) (r * 2)
        |> Shape2d.move move.x move.y


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        Tick frameStuff ->
            let
                world : World
                world =
                    model.world

                deltaT : Duration
                deltaT =
                    frameStuff.dt
                        -- Avoid jumps if focus is lost
                        |> min 200
                        |> Duration.milliseconds
            in
            ( { model
                | now = frameStuff.timestamp
                , world =
                    world
                        |> resetBall frameStuff model
                        |> applyGamepadInput model frameStuff
                        |> applySpeeds deltaT
                        |> applyAccelerations deltaT
              }
            , Effect.none
            )

        Resize w h ->
            ( { model
                | width = toFloat w
                , height = toFloat h
              }
            , Effect.none
            )

        LoadedTexture _ (Err _) ->
            ( model, Effect.none )

        LoadedTexture key (Ok texture) ->
            ( { model | textures = Dict.insert key texture model.textures }, Effect.none )


resetBall : FrameStuff -> Model -> System World
resetBall frameStuff model =
    System.update positionSpec
        (if
            List.any
                (\gamepad -> Gamepad.wasReleased gamepad Gamepad.B)
                frameStuff.gamepads
         then
            Component.set model.ball
                Vector2d.zero

         else
            identity
        )


applyGamepadInput : Model -> FrameStuff -> (World -> World)
applyGamepadInput model frameStuff =
    System.update accelerationSpec
        (Component.set
            model.ball
            (gamepadToAccelleration frameStuff.gamepads)
        )


gamepadToAccelleration : List Gamepad -> Vector2d MetersPerSecondSquared
gamepadToAccelleration gamepads =
    let
        scale : Float -> Acceleration.Acceleration
        scale n =
            Acceleration.metersPerSecondSquared <| n * 10000

        applyGamepad : Gamepad -> Vector2d MetersPerSecondSquared -> Vector2d MetersPerSecondSquared
        applyGamepad gamepad acc =
            acc
                |> Vector2d.plus
                    (Vector2d.fromRecord scale
                        (Gamepad.leftStickPosition gamepad)
                    )
    in
    List.foldl applyGamepad Vector2d.zero gamepads
        |> preventWiggle (scale 0.08)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize Resize


init : Flags -> ( Model, Effect )
init flags =
    let
        initialWorld : World
        initialWorld =
            { ecsConfig = Config.init
            , positionComponent = Component.empty
            , speedComponent = Component.empty
            , accelerationComponent = Component.empty
            }

        ( ball, world ) =
            initialWorld
                |> Entity.create ecsConfigSpec
                |> Entity.with ( positionSpec, Vector2d.zero )
                |> Entity.with ( speedSpec, Vector2d.zero )
                |> Entity.with ( accelerationSpec, Vector2d.zero )

        model : Model
        model =
            { world = world
            , ball = ball
            , now = flags.now
            , width = flags.width
            , height = flags.height
            , textures = Dict.empty
            }
    in
    ( model
    , [ ( Xbox.a.path, "img/" ++ Xbox.a.path )
      , ( Xbox.b.path, "img/" ++ Xbox.b.path )
      ]
        |> Effect.loadTextures
    )


time : Model -> Time.Posix
time { now } =
    now


onAnimationFrame : FrameStuff -> Msg
onAnimationFrame frameStuff =
    Tick frameStuff


loadedTexture : String -> Result Texture.Error Texture -> Msg
loadedTexture key content =
    LoadedTexture key content
