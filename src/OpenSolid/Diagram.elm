module OpenSolid.Diagram
    exposing
        ( vector2d
        , vector2dWith
        , vector3d
        , vector3dWith
        , direction2d
        , direction2dWith
        , direction3d
        , direction3dWith
        , point2d
        , point2dWith
        , point3d
        , point3dWith
        , frame2d
        , Frame2dOptions
        , frame2dWith
        , frame3d
        , Frame3dOptions
        , frame3dWith
        )

import Svg exposing (Svg)
import Svg.Attributes as Attributes
import OpenSolid.Svg as Svg
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Point2d as Point2d


arrow :
    List (Svg.Attribute msg)
    -> { tipLength : Float, tipWidth : Float }
    -> Point2d
    -> Vector2d
    -> Svg msg
arrow attributes { tipLength, tipWidth } point vector =
    case Vector2d.lengthAndDirection vector of
        Just ( length, direction ) ->
            let
                frame =
                    Frame2d
                        { originPoint = point
                        , xDirection = direction
                        , yDirection = Direction2d.perpendicularTo direction
                        }

                localPoint coordinates =
                    Point2d.placeIn frame (Point2d coordinates)

                tipPoint =
                    localPoint ( length, 0 )

                tipBasePoint =
                    localPoint ( length - tipLength, 0 )

                leftPoint =
                    localPoint ( length - tipLength, tipWidth / 2 )

                rightPoint =
                    localPoint ( length - tipLength, -tipWidth / 2 )

                stem =
                    LineSegment2d ( point, tipBasePoint )

                tip =
                    Triangle2d ( rightPoint, tipPoint, leftPoint )
            in
                Svg.g attributes
                    [ Svg.lineSegment2d [] stem
                    , Svg.triangle2d [] tip
                    ]

        Nothing ->
            Svg.text ""


vector2d : Point2d -> Vector2d -> Svg msg
vector2d =
    vector2dWith []


vector2dWith : List (Attribute msg) -> Point2d -> Vector2d -> Svg msg
vector2dWith attributes point vector =
    arrow (Attributes.fill "black" :: attributes)
        { tipLength = 0.24, tipWidth = 0.2 }
        point
        vector


vector3d : SketchPlane3d -> Point3d -> Vector3d -> Svg msg
vector3d =
    vector3dWith []


vector3dWith : List (Attribute msg) -> SketchPlane3d -> Point3d -> Vector3d -> Svg msg
vector3dWith viewPlane attributes point vector =
    vector2d attributes
        (Point3d.projectInto viewPlane point)
        (Vector3d.projectInto viewPlane vector)


direction2d : Point2d -> Direction2d -> Svg msg
direction2d =
    direction2dWith []


direction2dWith : List (Attribute msg) -> Point2d -> Direction2d -> Svg msg
direction2dWith attributes point direction =
    arrow (Attributes.fill "white" :: attributes)
        { tipLength = 0.2, tipWidth = 0.2 }
        point
        (Direction2d.toVector direction)


direction3d : SketchPlane3d -> Point3d -> Direction3d -> Svg msg
direction3d =
    direction3dWith []


direction3dWith : List (Attribute msg) -> SketchPlane3d -> Point3d -> Direction3d -> Svg msg
direction3dWith attributes viewPlane point direction =
    arrow (Attributes.fill "white" :: attributes)
        { tipLength = 0.2, tipWidth = 0.2 }
        (Point3d.projectInto viewPlane point)
        (Vector3d.projectInto viewPlane (Direction3d.toVector direction))


point2d : Point2d -> Svg msg
point2d =
    point2dWith []


point2dWith : List (Attribute msg) -> Point2d -> Svg msg
point2dWith attributes point =
    Svg.circle2d (Attributes.fill "black" :: attributes)
        (Circle2d { centerPoint = point, radius = 0.1 })


point3d : SketchPlane3d -> Point3d -> Svg msg
point3d =
    point3dWith []


point3dWith : List (Attribute msg) -> SketchPlane3d -> Point3d -> Svg msg
point3dWith attributes viewPlane point =
    point2d attributes (Point3d.projectInto viewPlane point)


originPoint2d : Point2d -> Svg msg
originPoint2d =
    originPoint2dWith []


originPoint2dWith : List (Attribute msg) -> Point2d -> Svg msg
originPoint2dWith attributes point =
    Svg.circle2d (Attributes.fill "white" :: attributes)
        (Circle2d { centerPoint = point, radius = 0.1 })


originPoint3d : SketchPlane3d -> Point3d -> Svg msg
originPoint3d =
    originPoint3dWith []


originPoint3dWith : List (Attribute msg) -> SketchPlane3d -> Point3d -> Svg msg
originPoint3dWith attributes viewPlane point =
    originPoint2dWith attributes (Point3d.projectInto viewPlane point)


axis2d : Axis2d -> Svg msg
axis2d =
    axis2dWith defaultAxisOptions


type alias AxisOptions msg =
    { originPointAttributes : List (Attribute msg)
    , directionAttributes : List (Attribute msg)
    , lineAttributes : List (Attribute msg)
    , groupAttributes : List (Attribute msg)
    , start : Float
    , end : Float
    }


defaultAxisOptions : AxisOptions msg
defaultAxisOptions =
    { originPointAttributes = []
    , directionAttributes = []
    , groupAttributes = []
    , start = -1
    , end = 2
    }


axis2dWith : AxisOptions msg -> Axis2d -> Svg msg
axis2dWith options axis =
    let
        originPoint =
            Axis2d.originPoint axis

        lineSegment =
            LineSegment2d
                ( Point2d.along axis options.start
                , Point2d.along axis options.end
                )

        lineAttributes =
            Attributes.strokeDasharray "0.1 0.1"
                :: Attributes.strokeWidth "0.03"
                :: options.lineAttributes
    in
        Svg.g options.groupAttributes
            [ Svg.lineSegment2d lineAttributes
                lineSegment
            , direction2dWith options.directionAttributes
                originPoint
                (Axis2d.direction axis)
            , originPoint2dWith options.originPointAttributes
                originPoint
            ]


axis3d : SketchPlane3d -> Axis3d -> Svg msg
axis3d =
    axis3dWith defaultAxisOptions


axis3dWith : AxisOptions msg -> SketchPlane3d -> Axis3d -> Svg msg
axis3dWith options viewPlane axis =
    let
        originPoint =
            Axis3d.originPoint axis

        lineSegment =
            LineSegment3d
                ( Point3d.along axis options.start
                , Point3d.along axis options.end
                )
                |> LineSegment3d.projectInto viewPlane

        lineAttributes =
            Attributes.strokeDasharray "0.1 0.1"
                :: Attributes.strokeWidth "0.025"
                :: options.lineAttributes
    in
        Svg.g options.groupAttributes
            [ Svg.lineSegment2d lineAttributes
                lineSegment
            , direction3dWith options.directionAttributes
                viewPlane
                originPoint
                (Axis3d.direction axis)
            , originPoint3dWith options.originPointAttributes
                viewPlane
                originPoint
            ]


frame2d : Frame2d -> Svg msg
frame2d frame =
    frame2dWith defaultFrame2dOptions


type alias Frame2dOptions msg =
    { originPointAttributes : List (Attribute msg)
    , xDirectionAttributes : List (Attribute msg)
    , yDirectionAttributes : List (Attribute msg)
    , groupAttributes : List (Attribute msg)
    }


defaultFrame2dOptions : Frame2dOptions msg
defaultFrame2dOptions =
    { originPointAttributes = []
    , xDirectionAttributes = []
    , yDirectionAttributes = []
    , groupAttributes = []
    }


frame2dWith : Frame2dOptions msg -> Frame2d -> Svg msg
frame2dWith options frame =
    let
        originPoint =
            Frame2d.originPoint frame
    in
        Svg.g options.groupAttributes
            [ direction2dWith options.xDirectionAttributes
                originPoint
                (Frame2d.xDirection frame)
            , direction2dWith options.yDirectionAttributes
                originPoint
                (Frame2d.yDirection frame)
            , originPoint2dWith options.originPointAttributes
                originPoint
            ]


frame3d : Frame3d -> Svg msg
frame3d =
    frame3dWith defaultFrame3dOptions


type alias Frame3dOptions msg =
    { originPointAttributes : List (Attribute msg)
    , xDirectionAttributes : List (Attribute msg)
    , yDirectionAttributes : List (Attribute msg)
    , zDirectionAttributes : List (Attribute msg)
    , groupAttributes : List (Attribute msg)
    }


defaultFrame3dOptions : Frame3dOptions msg
defaultFrame3dOptions =
    { originPointAttributes = []
    , xDirectionAttributes = []
    , yDirectionAttributes = []
    , zDirectionAttributes = []
    , groupAttributes = []
    }


frame3dWith : Frame3dOptions msg -> Frame3d -> Svg Never
frame3dWith options frame =
    let
        originPoint =
            Frame3d.originPoint frame
    in
        Svg.g options.groupAttributes
            [ direction3dWith options.xDirectionAttributes
                originPoint
                (Frame3d.xDirection frame)
            , direction3dWith options.yDirectionAttributes
                originPoint
                (Frame3d.yDirection frame)
            , direction3dWith options.zDirectionAttributes
                originPoint
                (Frame3d.zDirection frame)
            , originPoint3dWith options.originPointAttributes
                originPoint
            ]


plane3d : SketchPlane3d -> Plane3d -> Svg msg
plane3d viewPlane plane =
    plane3dWith defaultPlaneOptions


type alias PlaneOptions msg =
    { planeOrientation : Plane3d -> SketchPlane3d
    , originPointAttributes : List (Attribute msg)
    , normalDirectionAttributes : List (Attribute msg)
    , outlineAttributes : List (Attribute msg)
    , groupAttributes : List (Attribute msg)
    , outline : BoundingBox2d
    }


defaultPlaneOptions : PlaneOptions msg
defaultPlaneOptions =
    { planeOrientation = defaultPlaneOrientation
    , originPointAttributes = []
    , normalDirectionAttributes = []
    , outlineAttributes = []
    , groupAttributes = []
    , width = Float
    , height = Float
    }


defaultPlaneOrientation : Plane3d -> SketchPlane3d
defaultPlaneOrientation plane =
    let
        normalDirection =
            Plane3d.normalDirection plane

        tolerance =
            degrees 1.0e-3

        sketchPlane desiredXDirection =
            let
                defaultXDirection =
                    Direction3d.perpendicularTo normalDirection

                xDirection =
                    Direction3d.projectOnto plane desiredXDirection
                        |> Maybe.withDefault defaultXDirection

                defaultYDirection =
                    Direction3d.perpendicularTo xDirection

                normalVector =
                    Direction3d.toVector normalDirection

                xVector =
                    Direction3d.toVector xDirection

                yDirection =
                    Vector3d.crossProduct normalVector xVector
                        |> Vector3d.direction
                        |> Maybe.withDefault defaultYDirection
            in
                SketchPlane3d
                    { originPoint = Plane3d.originPoint plane
                    , xDirection = xDirection
                    , yDirection = yDirection
                    }

        normalIsIn direction =
            Direction3d.equalWithin tolerance direction normalDirection
    in
        if normalIsIn Direction3d.positiveZ then
            sketchPlane Direction3d.positiveX
        else if normalIsIn Direction3d.negativeZ then




plane3dWith : PlaneOptions msg -> SketchPlane3d -> Plane3d -> Svg msg
plane3dWith options viewPlane plane =
    let
        sketchPlane =
            options.sketchPlane plane

        point x y =
            Point2d ( x, y )
                |> Point2d.placeOnto sketchPlane
                |> Point3d.projectInto viewPlane

        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema options.outline

        outline =
            Polygon2d
                [ point minX minY
                , point maxX minY
                , point maxX maxY
                , point minX maxY
                ]

        originPoint =
            SketchPlane3d.originPoint sketchPlane

        normalDirection =
            SketchPlane3d.plane sketchPlane |> Plane3d.normalDirection
    in
        Svg.g []
            [ Svg.polygon2d
                [ Attributes.fill "none"
                , Attributes.strokeDasharray "3 3"
                , Attributes.strokeWidth "0.75"
                ]
                outline
            , direction3d originPoint normalDirection
            , originPoint3d originPoint
            ]


sketchPlane3d : SketchPlane3d -> SketchPlane3d -> Svg msg
sketchPlane3d =
    sketchPlane3dWith defaultSketchPlaneOptions


type alias SketchPlaneOptions msg =
    { originPointAttributes : List (Attribute msg)
    , xDirectionAttributes : List (Attribute msg)
    , yDirectionAttributes : List (Attribute msg)
    , outlineAttributes : List (Attribute msg)
    , groupAttributes : List (Attribute msg)
    , outlineBounds : BoundingBox2d
    }


defaultSketchPlaneOptions : SketchPlaneOptions msg
defaultSketchPlaneOptions =
    { originPointAttributes = []
    , xDirectionAttributes = []
    , yDirectionAttributes = []
    , outlineAttributes = []
    , groupAttributes = []
    , outlineBounds =
        BoundingBox2d
            { minX = -0.25
            , minY = -0.25
            , maxX = 1.2
            , maxY = 1.2
            }
    }


sketchPlane3dWith : SketchPlaneOptions msg -> SketchPlane3d -> SketchPlane3d -> Svg msg
sketchPlane3dWith options viewPlane sketchPlane =
    let
        point x y =
            Point2d ( x, y )
                |> Point2d.placeOnto sketchPlane
                |> Point3d.projectInto viewPlane

        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema options.outlineBounds

        outline =
            Polygon2d
                [ point minX minY
                , point maxX minY
                , point maxX maxY
                , point minX maxY
                ]

        originPoint =
            SketchPlane3d.originPoint sketchPlane

        outlineAttributes =
            Attributes.fill "none"
                :: Attributes.strokeDasharray "3 3"
                :: Attributes.strokeWidth "0.75"
                :: options.outlineAttributes
    in
        Svg.g options.groupAttributes
            [ Svg.polygon2d outlineAttributes
                outline
            , direction3dWith options.xDirectionAttributes
                originPoint
                (SketchPlane3d.xDirection sketchPlane)
            , direction3dWith options.yDirectionAttributes
                originPoint
                (SketchPlane3d.yDirection sketchPlane)
            , originPoint3dWith options.originPointAttributes
                originPoint
            ]
