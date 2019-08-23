import "raymarcher"
import "scene"

let glass : material
  = {colour=col(0.6, 0.8, 0.6), light=false, reflectivity=1.0
     , transparent=#just 1.9}

let infiniteSpheresScene (p : vec3) : (f32, material) =
  let pglass = vec3.map (flip fmod 8) p
  let pblue = (vec(fmod p.x 8, fmod (p.y + 0) 8, fmod (p.z + 4) 8))
  let plights = (vec(fmod (p.x + 4) 8, fmod (p.y + 0) 8, fmod (p.z + 0) 8))
  let pmirror = (vec(fmod (p.x + 4) 8, fmod (p.y + 0) 8, fmod (p.z + 4) 8))
  let m = mirror (vec(0.9, 0.2, 0.2))
  let blueGlass = transparent (vec(0.9, 0.8, 1.0)) 1.6
  let spheres =
    minArr [(vec3.norm (pglass vec3.- vec(4,4,4)) - 1, m),
            (vec3.norm (pblue vec3.- vec(4,4,4)) - 1, blueGlass),
            (vec3.norm (plights vec3.- vec(4,4,4)) - 1, mirror (vec(0.2, 0.9, 0.3))),
            (vec3.norm (pmirror vec3.- vec(4,4,4)) - 1, blueGlass)]
  in spheres

let globalLight (rd : vec3) : vec3 =
  let sunDir = vec(0, 1, -8)
  let sunLight = vec(0.4, 0.2, 0.4)
  let contribution = f32.max 0 (vec3.dot sunDir rd)
  in vec3.scale contribution sunLight

let camera : camera =
  let camPos = vec(-4,5.3,-6)
  in #simple { camPos
             , lookAt= camPos vec3.+ vec(0.3,0.0,1)}

entry main : [height][width][3]u8 =
  raymarcher {sdf=infiniteSpheresScene, globalLight, camera}
