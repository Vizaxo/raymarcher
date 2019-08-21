import "lib/github.com/athas/vector/vspace"

let tau = 2 * f32.pi

module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector
type col3 = vec3.vector

type sphere = {centre: vec3, radius: f32}
type plane =  {norm: vec3, d: f32}
type object = #sphere sphere | #plane

let vec (x, y, z) : vec3 = {x, y, z}
let col (r, g, b) : col3 = {x=r, y=g, z=b}

let height : i32 = 256
let width : i32 = 512

let maxSteps : i32 = 100
let maxBounces : i32 = 3
let maxRays : i32 = 1000
let epsilon : f32 = 0.1

type material =
  { colour: col3
  , reflectivity: f32
  , light: bool}

let diffuse colour : material = {colour, light=false, reflectivity=0.2}
let light colour : material = {colour, light=true, reflectivity=0.2}
let mirror colour : material = {colour, light=false, reflectivity=0.95}
let black = col(0.0, 0.0, 0.0)
let white = col(0.5, 0.5, 0.5)
let blue = col(0.05, 0.05, 0.3)
let red = col(20.0, 0.3, 1.3)
let skyColour = col(0.1, 0.1, 0.5)
let sunColour = col(5, 5, 4)

type hit = #no_hit | #hit {hitPos: vec3, mat: material}

let scene : [](object, material) =
  [(#plane, diffuse white),
   (#sphere {centre=vec(0, 3, 0), radius=1}, mirror white),
   (#sphere {centre=vec(1, 1.0, -1), radius=1}, diffuse blue),
   (#sphere {centre=vec(0, 0.5, 1), radius=1}, mirror white)]
   --(#sphere {centre=vec(-15, 0.5, 1), radius=10}, mirror white)]

let lerp a b x = (vec3.scale (1 - x) a) vec3.+ (vec3.scale x b)

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

let fst (a, _) = a
let snd (_, b) = b

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

--from https://amindforeverprogramming.blogspot.com/2013/07/random-floats-in-glsl-330.html
let hash (b : u32) : u32 =
  let b = b + (b << 10)
  let b = b ^ (b >> 6)
  let b = b + (b << 3)
  let b = b ^ (b >> 11)
  let b = b + (b << 15)
  in b
let f32rand (f : f32) =
  let mantissa = 0x007FFFFF
  let one = 0x3F800000
  let h = hash <| f32.to_bits f
  let h = h & mantissa
  let h = h | one
  in f32.from_bits h - 1.0

let rand x y ray bounce f : f32 =
  let seed : f32 = f32rand((f32rand (x*5344 + y) * f32.i32 maxRays + f32.i32 ray) * f32.i32 maxBounces + f32.i32 bounce) + f32.i32 f
  in f32rand seed

let sampleSphere x y ray bounce : vec3 =
  let u1 = rand x y ray bounce 0
  let u2 = rand x y ray bounce 10
  let r = f32.sqrt(1.0 - u1 * u1)
  let phi = tau * u2
  in vec3.normalise(vec(f32.cos phi * r, f32.sin phi * r, 2.0 * u1))

let sampleHemisphere n x y ray bounce : vec3 =
  let v = sampleSphere x y ray bounce
  in if vec3.dot n v < 0.0
     then vec3.map (f32.negate) v
     else v

let skyLighting rd colour =
  let sunDir = vec3.normalise (vec(8,1,-5))
  let skyDir = vec(0,1,0)
  let sunLight = vec3.scale (f32.max 0 (vec3.dot rd sunDir)) sunColour
  let skyLight = vec3.scale (f32.max 0 (vec3.dot rd skyDir)) skyColour
  in colour vec3.* (sunLight vec3.+ skyLight)

let ray ray x y (rd : vec3) (ro : vec3) : col3 =
  let bounces = 0
  let colour = col(1.0, 1.0, 1.0)
  let break = false
  let (bounces, colour, _, _, _) =
    loop (bounces, colour, rd, ro, break) while (bounces < maxBounces && !break) do
         match march rd ro
         case #no_hit -> (bounces, skyLighting rd colour, rd, ro, true)
         case #hit {hitPos, mat} ->
      if mat.light
      then (bounces, colour vec3.* mat.colour, vec(0,0,0), vec(0,0,0), true)
      else
        let hitNorm = getNorm(hitPos)
        let reflected = reflect rd hitNorm
        let scattered = sampleHemisphere hitNorm x y ray bounces
        let newRay = lerp scattered reflected (mat.reflectivity)
        in (bounces+1, colour vec3.* mat.colour, newRay, hitPos vec3.+ (vec3.scale (10*epsilon) newRay), false)
  in if bounces != maxBounces then colour else black

let jitterRay i x y rd ro =
  let u1 = rand x y i 0 2
  let u2 = rand x y i 0 3
  let rd = vec3.normalise <| rd vec3.+ vec(u1 / f32.i32 width, u2 / f32.i32 height, 0)
  in ray i x y rd ro

let pixel x y rd ro : col3 =
  let mr = f32.i32 maxRays
  in reduce (vec3.+) (col(0,0,0)) (map (\i -> jitterRay i x y rd ro) (iota maxRays)) vec3./ col(mr,mr,mr)

let gammaCorrect c =
  vec3.map (f32.** 0.45) c

let shader (y: f32) (x: f32) : col3 =
  let camPos = vec(0, 3, -10)
  let lookAt = vec(0, 0, 0)
  let filmCentre = vec3.normalise(lookAt vec3.- camPos)
  let filmPos = filmCentre vec3.+ vec(x*10, y*10, 0)
  let rd = vec3.normalise(filmPos vec3.- camPos)
  let ro = camPos
  in gammaCorrect <| pixel x y rd ro

let bounds lower upper x : f32 = if x < lower then lower else (if x > upper then upper else x)

let packCol ({x=r, y=g, z=b} : col3) : [3]u8 = map (u8.f32 <-< bounds 0 255 <-< (* 255)) [r, g, b]

let canvas (y: i32) (x: i32) : [3]u8 =
  packCol <| (shader (-((f32.i32 y) / (f32.i32 height) - 0.5)) ((f32.i32 x) / (f32.i32 width) - 0.5))

entry main : [height][width][3]u8 =
  map (\x -> (map (canvas x) (iota width))) (iota height)
