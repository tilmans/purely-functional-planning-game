module Quad exposing (card, bg, c0, c1, c2, c3, c5, c8, c13)

import WebGL exposing (..)
import Math.Vector2 as V2 exposing (..)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


verteces =
    [ vec3 0.25 0 0
    , vec3 0.25 0.4 0
    , vec3 0 0.4 0
    , vec3 0 0 0
    ]


c0 =
    [ vec2 0.25 0.5
    , vec2 0.25 1
    , vec2 0 1
    , vec2 0 0.5
    ]


c1 =
    [ vec2 0.5 0.5
    , vec2 0.5 1
    , vec2 0.25 1
    , vec2 0.25 0.5
    ]


c2 =
    [ vec2 0.75 0.5
    , vec2 0.75 1
    , vec2 0.5 1
    , vec2 0.5 0.5
    ]


c3 =
    [ vec2 1 0.5
    , vec2 1 1
    , vec2 0.75 1
    , vec2 0.75 0.5
    ]


c5 =
    [ vec2 0.25 0
    , vec2 0.25 0.5
    , vec2 0 0.5
    , vec2 0 0
    ]


c8 =
    [ vec2 0.5 0
    , vec2 0.5 0.5
    , vec2 0.25 0.5
    , vec2 0.25 0
    ]


c13 =
    [ vec2 0.75 0
    , vec2 0.75 0.5
    , vec2 0.5 0.5
    , vec2 0.5 0
    ]


card : Vec2 -> Texture -> List Vec2 -> Entity
card center texture card =
    entity vertexShader fragmentShader (mesh center card) { texture = texture }


bg =
    entity bgVS bgFS bgmesh {}


mesh : Vec2 -> List Vec2 -> Mesh Vertex
mesh center card =
    let
        v3 =
            vec3 (V2.getX center) (V2.getY center) 0

        trans =
            makeTranslate v3

        translated =
            List.map (transform trans) verteces

        att =
            List.map2 (\v c -> Vertex v c) translated card
    in
        indexedTriangles att [ ( 0, 1, 2 ), ( 2, 3, 0 ) ]


bgmesh =
    indexedTriangles
        [ { position = vec3 -1 -1 0 }
        , { position = vec3 1 -1 0 }
        , { position = vec3 1 1 0 }
        , { position = vec3 -1 1 0 }
        ]
        [ ( 0, 1, 2 )
        , ( 0, 2, 3 )
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


bgVS =
    [glsl|
        attribute vec3 position;

        void main () {
            gl_Position = vec4(position, 1.0);
        }
|]


bgFS =
    [glsl|
        precision mediump float;

        void main () {
            gl_FragColor = vec4(1,0,0,1);
        }
|]
