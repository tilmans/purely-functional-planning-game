module Quad exposing (card)

import WebGL exposing (..)
import Math.Vector2 exposing (..)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


card : Vec2 -> Texture -> Entity
card center texture =
    entity vertexShader fragmentShader (mesh center) { texture = texture }


mesh : Vec2 -> Mesh Vertex
mesh center =
    indexedTriangles
        [ Vertex (vec3 1 0 0) (vec2 1 0)
        , Vertex (vec3 1 1 0) (vec2 1 1)
        , Vertex (vec3 0 1 0) (vec2 0 1)
        , Vertex (vec3 0 0 0) (vec2 0 0)
        ]
        [ ( 0, 1, 2 )
        , ( 2, 3, 0 )
        ]



-- SHADERS


vertexShader : Shader Vertex { texture : Texture } { vcoord : Vec2 }
vertexShader =
    [glsl|
    attribute vec3 position;
    attribute vec2 coord;
    varying vec2 vcoord;

    void main () {
        gl_Position = vec4(position, 1.0);
        vcoord = coord;
    }
|]


fragmentShader : Shader {} { u | texture : Texture } { vcoord : Vec2 }
fragmentShader =
    [glsl|
    precision mediump float;
    uniform sampler2D texture;
    varying vec2 vcoord;

    void main () {
        gl_FragColor = texture2D(texture, vcoord);
    }
|]
