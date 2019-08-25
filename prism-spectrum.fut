import "raymarcher"
import "scene"

let prismDist (p : vec3) : f32 =
  let p = vec(p.x - 0.0, p.y, p.z)
  let bottom = -(p.y - 0.8)
  let right = ((2/3 * p.y) + p.x)
  let left = ((2/3 * p.y) - p.x - 3)
  let back = (p.z - 1)
  let front = (-p.z - 3)
  let bottomLeft = -((2/3 * p.y) + p.x + 1)
  in reduce f32.max (-1/0) [bottom, right, left, front, back]

let cornellBoxScene (p : vec3) : (f32, material) =
  let floor = (p.y, diffuse (col(1.0, 1.0, 1.0)))
  let barLight =
    (reduce f32.max (-1) [p.y - 1.2999, (-p.y + 1.2999), p.x + 3]
    , light (col(30, 30, 30)))
  let lightBlocker = ((2/3 * p.x) + p.y + 0.9, diffuse black)

  let prism = (prismDist p, glass)
  let scene = minArr [floor, barLight, prism, lightBlocker]
  in scene

let camera : camera =
  #simple {camPos=vec(-1.5, 1.5, -5)
          , lookAt=vec(-1.5, 0.0, 0)}

let globalLight (rd : vec3) =
  vec(0,0,0)

entry main : [height][width][3]u8 =
  raymarcher {sdf=cornellBoxScene, globalLight, camera}
