import "raymarcher"
import "scene"

let glass : material
  = {colour=col(0.6, 0.8, 0.6), light=false, reflectivity=1.0
     , transparent=#just 1.9}

let infiniteSpheresScene (p : vec3) : (f32, material) =
  let pglass = vec3.map (flip fmod 8) p
  let pblue = (vec(fmod p.x 8, fmod (p.y + 8) 16, fmod (p.z + 4) 8))
  let pred = (vec(fmod p.x 8, fmod p.y 16, fmod (p.z + 4) 8))
  let pmirror = vec3.map (flip fmod 8) (vec(p.x, p.y + 4, p.z))
  let plights = vec3.map (flip fmod 16) (vec(p.x, p.y + 4, p.z + 4))
  let spheres =
    minArr [(vec3.norm (pglass vec3.- vec(4,4,4)) - 1, glass),
            (vec3.norm (pblue vec3.- vec(4,4,4)) - 1, specular (vec(0.2,0.4,0.9))),
            (vec3.norm (plights vec3.- vec(4,4,4)) - 1, light (vec(1, 1, 1))),
            (vec3.norm (pmirror vec3.- vec(4,4,4)) - 1, mirror white)]
  in spheres

let globalLight (rd : vec3) =
  vec(0, 0, 0)

let camera : camera =
  let camPos = vec(-4,5.3,-6)
  in #simple { camPos
             , lookAt= camPos vec3.+ vec(0.0,0.0,1)}

entry main : [height][width][3]u8 =
  raymarcher {sdf=infiniteSpheresScene, globalLight, camera}
