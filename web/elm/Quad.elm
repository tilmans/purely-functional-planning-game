module Quad exposing (quad, red, blue, card)

import WebGL exposing (..)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)


width =
    0.5


height =
    1


mesh size color =
    let
        minx =
            -size

        miny =
            size

        maxx =
            size

        maxy =
            -size

        ( r, g, b ) =
            color
    in
        TriangleStrip
            [ { position = (vec3 minx miny 0), color = (vec3 r g b) }
            , { position = (vec3 maxx miny 0), color = (vec3 r g b) }
            , { position = (vec3 maxx maxy 0), color = (vec3 r g b) }
            , { position = (vec3 minx miny 0), color = (vec3 r g b) }
            , { position = (vec3 minx maxy 0), color = (vec3 r g b) }
            ]


cardmesh ( x, y ) =
    let
        offx =
            width / 2

        offy =
            height / 2

        minx =
            x - offx

        maxx =
            x + offx

        miny =
            y - offy

        maxy =
            y + offy
    in
        TriangleStrip
            [ { position = (vec3 minx miny 0), color = (vec3 0 1 0) }
            , { position = (vec3 maxx miny 0), color = (vec3 1 0 0) }
            , { position = (vec3 maxx maxy 0), color = (vec3 1 0 0) }
            , { position = (vec3 minx miny 0), color = (vec3 1 0 0) }
            , { position = (vec3 minx maxy 0), color = (vec3 1 0 0) }
            ]


quad size color =
    [ render vertexShader fragmentShader (mesh size color) {} ]


card : ( Float, Float ) -> List WebGL.Renderable
card center =
    [ (render vertexShader fragmentShader (cardmesh center)) {} ]



-- SHADERS


vertexShader : Shader { attr | position : Vec3, color : Vec3 } {} { vcolor : Vec3 }
vertexShader =
    [glsl|
    attribute vec3 position;
    attribute vec3 color;

    varying vec3 vcolor;

    void main () {
        gl_Position = vec4(position, 1.0);
        vcolor = color;
    }
|]


fragmentShader : Shader {} {} { vcolor : Vec3 }
fragmentShader =
    [glsl|
    precision mediump float;

    varying vec3 vcolor;

    void main () {
        gl_FragColor = vec4(vcolor,1);
    }
|]


red =
    ( 1, 0, 0 )


blue =
    ( 0, 0, 1 )
