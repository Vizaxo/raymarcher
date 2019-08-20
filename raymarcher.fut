import "lib/github.com/athas/vector/vspace"

module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector
type col3 = vec3.vector

type sphere = {centre: vec3, radius: f32}
type plane =  {norm: vec3, d: f32}
type object = #sphere sphere | #plane

let vec (x, y, z) : vec3 = {x, y, z}
let col (r, g, b) : col3 = {x=r, y=g, z=b}

let scene : []object = [#plane, #sphere {centre=vec(0, 1, 0), radius=1}]

let getDist (p : vec3) (obj : object) : f32 =
  match obj
  case #sphere {centre, radius} ->
    vec3.norm(centre vec3.- p) - radius
  case #plane ->
    p.y

let min (x : f32) (y : f32) : f32 = if x < y then x else y

let sceneDist (p : vec3) : f32 =
  foldr min f32.highest (map (getDist p) scene)

let maxSteps : i32 = 1000
let epsilon : f32 = 0.01

let getNorm p : vec3 =
  let d = sceneDist p
  let n = vec(d - sceneDist(p vec3.- vec(epsilon, 0, 0))
             ,d - sceneDist(p vec3.- vec(0, epsilon, 0))
             ,d - sceneDist(p vec3.- vec(0, 0, epsilon)))
  in vec3.normalise n

let getLight p : f32 =
  let lightPos = vec (-2, 3, -2)
  let lightDir = vec3.normalise(lightPos vec3.- p)
  let norm = getNorm p
  in vec3.dot norm lightDir

type hit = #no_hit | #hit {hitPos: vec3}

let ray rd ro : hit =
  let steps = 0
  let dist : f32 = -1
  let d : f32 = 1
  let (_, _, d, ro) : (i32, f32, f32, vec3) =
    loop (steps, dist, d, ro) while (steps < maxSteps && (dist == -1 || d >= epsilon)) do
    let d = sceneDist ro
    in if (d >= 0)
       then
         (steps+1, dist + d, d, ro vec3.+ (vec3.scale d rd))
       else
         (steps+1, dist, d, ro) --TODO: what to do if d is negative?
  in if (d < epsilon) then #hit {hitPos=ro} else #no_hit

let shader (y: f32) (x: f32) : col3 =
  let camPos = vec(0, 3, -10)
  let lookAt = vec(0, 0, 0)
  let filmCentre = vec3.normalise(lookAt vec3.- camPos)
  let filmPos = filmCentre vec3.+ vec(x*10, y*10, 0)
  let rd = vec3.normalise(filmPos vec3.- camPos)
  let ro = camPos
  let hit = ray rd ro
  in match hit
     case #no_hit -> col(0, 0, 1)
     case #hit {hitPos} -> let c = getLight hitPos in col(c, c, c)

let bounds lower upper x : f32 = if x < lower then lower else (if x > upper then upper else x)

let packCol ({x=r, y=g, z=b} : col3) : [3]u8 = map (u8.f32 <-< bounds 0 255 <-< (* 255)) [r, g, b]

let canvas (y: i32) (x: i32) : [3]u8 =
  packCol <| (shader (-((f32.i32 y) / 256.0 - 0.5)) ((f32.i32 x) / 512.0 - 0.5))

entry main : [256][512][3]u8 =
  map (\x -> (map (canvas x) (iota 512))) (iota 256)
