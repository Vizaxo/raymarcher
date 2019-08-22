import "raymarcher"
import "scene"

let cornellBoxScene (p : vec3) : (f32, material) =
  let leftWall = (p.x + 3.0, diffuse (col(0.2,0.8,0.2)))
  let rightWall = (-p.x + 3.0, diffuse (col(0.2,0.2,0.8)))
  let ceiling = (-p.y + 6.0, diffuse (col(0.8, 0.8, 0.8)))
  let floor = (p.y, diffuse (col(0.8,0.8,0.8)))
  let back = (-p.z + 3.0, diffuse (col(0.8, 0.8, 0.2)))
  let front = (p.z + 11.0, diffuse (col(0.2, 0.2, 0.2)))
  let walls = reduce minMat (f32.highest, diffuse black)
                     [leftWall, rightWall, back, front, ceiling, floor]
  let ceilLight = (sphereDist p (vec(0, 605.997, 0)) 600, (light (col(5.0, 5.0, 5.0))))

  let mirrorSphere =
    (f32.max
     (sphereDist p (vec(-2, 1, -1.5)) 1)
     (-sphereDist p (vec(-1.5, 1, -1.7)) 1)
     --(p.x + 1.8)
    , mirror white)
  let redSphere =
    (sphereDist p (vec(-0.5, 1, 1.5)) 1, diffuse red)
  let spheres = minArr [mirrorSphere, redSphere]

  let scene = minArr [spheres, walls, ceilLight]
  let glassSphere =
    (f32.max
     (sphereDist p (vec(1.5,1,-2)) 1)
     (p.y - 1.5))
  in minMat scene (glassSphere, glass)

let camera : camera =
  #simple {camPos=vec(0, 3, -8)
          , lookAt=vec(0, 2, 0)}

let globalLight (rd : vec3) =
  vec(0,0,0)

entry main : [height][width][3]u8 =
  raymarcher {sdf=cornellBoxScene, globalLight, camera}
