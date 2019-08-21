import "utils"
import "material"

type camera = #simple {camPos: vec3, lookAt: vec3}
type scene = {sdf: vec3 -> (f32, material),
              camera: camera}

let sphereDist (p : vec3) centre radius : f32 =
  vec3.norm(centre vec3.- p) - radius

let minMat ((x, xmat) : (f32, material)) ((y, ymat) :  (f32, material)) : (f32, material)
  = if x < y then (x,xmat) else (y,ymat)
let maxMat ((x, xmat) : (f32, material)) ((y, ymat) :  (f32, material)) : (f32, material)
  = if x > y then (x,xmat) else (y,ymat)

let minArr (os : [](f32, material)) : (f32, material) =
  reduce minMat (f32.highest, diffuse black) os
