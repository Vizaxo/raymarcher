import "lib/github.com/athas/vector/vspace"

module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector
type col3 = vec3.vector

type sphere = {centre: vec3, radius: f32}
type plane =  {norm: vec3, d: f32}
type object = #sphere sphere | #plane

let vec (x, y, z) : vec3 = {x, y, z}
let col (r, g, b) : col3 = {x=r, y=g, z=b}

type material =
  { colour: col3
  , light: bool}

let diffuse colour = {colour, light=false}
let light colour = {colour, light=true}
let black = col(0.0, 0.0, 0.0)
let white = col(1.0, 1.0, 1.0)
let blue = col(0.2, 0.4, 0.95)
let red = col(1.0, 0.1, 0.15)

type hit = #no_hit | #hit {hitPos: vec3, mat: material}

let scene : [](object, material) =
  [(#plane, diffuse blue),
   (#sphere {centre=vec(0, 1, 0), radius=1}, light red)]

let getDist (p : vec3) (obj : object, mat : material) : (f32, material) =
  (match obj
  case #sphere {centre, radius} ->
    vec3.norm(centre vec3.- p) - radius
  case #plane ->
    p.y, mat)

let min (x : f32) (y : f32) : f32 = if x < y then x else y
let minMat ((x, xmat) : (f32, material)) ((y, ymat) :  (f32, material)) : (f32, material)
  = if x < y then (x,xmat) else (y,ymat)

let sceneDist (p : vec3) : (f32, material) =
  reduce minMat (f32.highest, diffuse black) (map (getDist p) scene)

let fst (a, b) = a
let snd (a, b) = b

let maxSteps : i32 = 1000
let maxBounces : i32 = 3
let epsilon : f32 = 0.01

let getNorm p : vec3 =
  let d = fst (sceneDist p)
  let n = vec(d - fst(sceneDist(p vec3.- vec(epsilon, 0, 0)))
             ,d - fst(sceneDist(p vec3.- vec(0, epsilon, 0)))
             ,d - fst(sceneDist(p vec3.- vec(0, 0, epsilon))))
  in vec3.normalise n

let march (rd : vec3) (ro : vec3) : hit =
  let steps = 0
  let dist : f32 = -1
  let d : f32 = 1
  let mat = diffuse black
  let (_, _, d, ro, mat) : (i32, f32, f32, vec3, material) =
    loop (steps, dist, d, ro, mat) while (steps < maxSteps && (dist == -1 || d >= epsilon)) do
    let (d, mat) = sceneDist ro
    in if (d >= 0)
       then
         (steps+1, dist + d, d, ro vec3.+ (vec3.scale d rd), mat)
       else
         (steps+1, dist, d, ro, mat) --TODO: what to do if d is negative?
  in if (d < epsilon) then #hit {hitPos=ro, mat} else #no_hit

let reflect (d : vec3) (n : vec3) : vec3 =
  d vec3.- vec3.scale (2 * vec3.dot d n) n

let ray (rd : vec3) (ro : vec3) : col3 =
  let bounces = 0
  let colour = col(1.0, 1.0, 1.0)
  let break = false
  let (bounces, colour, _, _, _) =
    loop (bounces, colour, rd, ro, break) while (bounces < maxBounces && !break) do
         match march rd ro
         case #no_hit -> (bounces+1, col(0, 0, 0), rd, ro, false)
         case #hit {hitPos, mat} ->
      if mat.light
      then (bounces, colour vec3.* mat.colour, vec(0,0,0), vec(0,0,0), true)
      else
        let hitNorm = getNorm(hitPos)
        let reflected = reflect rd hitNorm
        in (bounces+1, colour vec3.* mat.colour, reflected, hitPos vec3.+ (vec3.scale (2*epsilon) reflected), false)
  let finalColour = if bounces != maxBounces then colour else col(0, 0, 0)
  in finalColour

let shader (y: f32) (x: f32) : col3 =
  let camPos = vec(0, 3, -10)
  let lookAt = vec(0, 0, 0)
  let filmCentre = vec3.normalise(lookAt vec3.- camPos)
  let filmPos = filmCentre vec3.+ vec(x*10, y*10, 0)
  let rd = vec3.normalise(filmPos vec3.- camPos)
  let ro = camPos
  let hit = ray rd ro
  in hit

let bounds lower upper x : f32 = if x < lower then lower else (if x > upper then upper else x)

let packCol ({x=r, y=g, z=b} : col3) : [3]u8 = map (u8.f32 <-< bounds 0 255 <-< (* 255)) [r, g, b]

let canvas (y: i32) (x: i32) : [3]u8 =
  packCol <| (shader (-((f32.i32 y) / 256.0 - 0.5)) ((f32.i32 x) / 512.0 - 0.5))

entry main : [256][512][3]u8 =
  map (\x -> (map (canvas x) (iota 512))) (iota 256)
