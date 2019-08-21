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

let slow : bool = true
let height : i32 = if slow then 400 else 100
let width : i32 = if slow then 600 else 100

let maxSteps : i32 = if slow then 100 else 50
let maxBounces : i32 = if slow then 50 else 20
let maxRays : i32 = if slow then 4000 else 500
let epsilon : f32 = 0.001

type maybe 't = #nothing | #just t

type material =
  { colour: col3
  , reflectivity: f32
  , light: bool
  , transparent: maybe f32}

let diffuse colour : material
  = {colour, light=false, reflectivity=0.2, transparent=#nothing}
let light colour : material
  = {colour, light=true, reflectivity=0.2, transparent=#nothing}
let mirror colour : material
  = {colour, light=false, reflectivity=0.95, transparent=#nothing}
let glass : material
  = {colour=col(1.0, 1.0, 1.0), light=false, reflectivity=1.0
     , transparent=#just 2}
let black = col(0.0, 0.0, 0.0)
let white = col(0.5, 0.5, 0.5)
let blue = col(0.05, 0.05, 0.3)
let red = col(1.0, 0.1, 0.1)
let skyColour = col(0.1, 0.1, 0.5)
let sunColour = col(5, 5, 4)

type hit = #no_hit | #hit {hitPos: vec3, mat: material, insideObj: bool}

let lerp a b x = (vec3.scale (1 - x) a) vec3.+ (vec3.scale x b)

let sphereDist (p : vec3) centre radius : f32 =
  vec3.norm(centre vec3.- p) - radius

let getDist (p : vec3) (obj : object, mat : material) : (f32, material) =
  (match obj
  case #sphere {centre, radius} ->
    sphereDist p centre radius
  case #plane ->
    p.y, mat)

let minMat ((x, xmat) : (f32, material)) ((y, ymat) :  (f32, material)) : (f32, material)
  = if x < y then (x,xmat) else (y,ymat)
let maxMat ((x, xmat) : (f32, material)) ((y, ymat) :  (f32, material)) : (f32, material)
  = if x > y then (x,xmat) else (y,ymat)

let minArr (os : [](f32, material)) : (f32, material) = reduce minMat (f32.highest, diffuse black) os

let sceneDistSDF (p : vec3) : (f32, material) =
  let leftWall = (p.x + 3.0, diffuse (col(0.2,0.8,0.2)))
  let rightWall = (-p.x + 3.0, diffuse (col(0.2,0.2,0.8)))
  let ceiling = (-p.y + 6.0, diffuse (col(0.8, 0.8, 0.8)))
  let floor = (p.y, diffuse (col(0.8,0.8,0.8)))
  let back = (-p.z + 3.0, diffuse (col(0.8, 0.8, 0.2)))
  let front = (p.z + 11.0, diffuse (col(0.2, 0.2, 0.2)))
  let walls = reduce minMat (f32.highest, diffuse black)
                     [leftWall, rightWall, back, front, ceiling, floor]
  let ceilLight = getDist p (#sphere {centre=vec(0, 605.997, 0), radius=600}, light (col(5.0, 5.0, 5.0)))

  let mirrorSphere =
    (f32.max
     (sphereDist p (vec(-2, 1, -1.5)) 1)
     (-sphereDist p (vec(-1.5, 1, -1.7)) 1)
     --(p.x + 1.8)
    , mirror white)
  let redSphere =
    getDist p (#sphere {centre=vec(-0.5, 1, 1.5), radius=1}, diffuse red)
  let spheres = minArr [mirrorSphere, redSphere]

  let scene = minArr [spheres, walls, ceilLight]
  let glassSphere =
    (f32.max
     (sphereDist p (vec(1.5,1,-2)) 1)
     (p.y - 1.5))
  in minMat scene (glassSphere, glass)

let fst (a, _) = a
let snd (_, b) = b

let getNorm p : vec3 =
  let d = vec(fst(sceneDistSDF(p vec3.+ vec(epsilon, 0, 0)))
             ,fst(sceneDistSDF(p vec3.+ vec(0, epsilon, 0)))
             ,fst(sceneDistSDF(p vec3.+ vec(0, 0, epsilon))))
  let n = d vec3.- vec(fst(sceneDistSDF(p vec3.- vec(epsilon, 0, 0)))
                      ,fst(sceneDistSDF(p vec3.- vec(0, epsilon, 0)))
                      ,fst(sceneDistSDF(p vec3.- vec(0, 0, epsilon))))
  in vec3.normalise n

let march (rd : vec3) (ro : vec3) : hit =
  let steps = 0
  let dist : f32 = -1
  let d : f32 = 1
  let mat = diffuse black
  let insideObj = false
  let (_, _, d, ro, mat, insideObj) : (i32, f32, f32, vec3, material, bool) =
    loop (steps, dist, d, ro, mat, insideObj) while (steps < maxSteps && (dist == -1 || d >= epsilon)) do
    let (d, mat) = sceneDistSDF ro
    let insideObj = if d < 0 then true else false
    -- use unsigned distance to allow marching through objects during
    -- refraction
    let d = f32.abs d
    in (steps+1, dist + d, d, ro vec3.+ (vec3.scale d rd), mat, insideObj)
  in if (d < epsilon) then #hit {hitPos=ro, mat, insideObj} else #no_hit

let reflect (d : vec3) (n : vec3) : vec3 =
  d vec3.- vec3.scale (2 * vec3.dot d n) n

let vinvert v : vec3 =
  vec3.map (* (-1)) v

-- Snell's law for calculating the angle of refraction
let snell rd surfaceNorm n1 n2 : vec3 =
  let normal = if vec3.dot rd surfaceNorm <= 0
               then surfaceNorm
               else vinvert surfaceNorm
  let r = n1 / n2
  let rootTerm = 1 - (r*r) * vec3.quadrance (vec3.cross normal rd)
  in (vec3.scale r (vec3.cross normal (vec3.cross (vinvert normal) rd)))
     vec3.- (vec3.scale (f32.sqrt rootTerm) normal)

-- Schlick's approximation of the Fresnel factor for calulating the
-- contribution ratio of reflected to refracted light
let schlick n1 n2 costheta : f32 =
  let r0 = ((n1 - n2) / (n1 + n2)) ** 2
  in r0 + (1 - r0) * ((1 - costheta) ** 5)

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

let normalTowards n rd =
  if vec3.dot n rd < 0.0
  then vinvert rd
  else rd

let sampleHemisphere n x y ray bounce : vec3 =
  let v = sampleSphere x y ray bounce
  in normalTowards n v

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
         case #no_hit -> (bounces, col(0,0,0), rd, ro, true)
         case #hit {hitPos, mat, insideObj} ->
      if mat.light
      then (bounces, colour vec3.* mat.colour, vec(0,0,0), vec(0,0,0), true)
      else
        let hitNorm = getNorm hitPos
        let perfectReflect = reflect rd hitNorm
        let scattered = sampleHemisphere hitNorm x y ray bounces
        let reflected = lerp scattered perfectReflect (mat.reflectivity)
        let newRay =
          match mat.transparent
          case #nothing ->
            reflected
          case #just ri ->
            let costheta = vec3.dot (normalTowards hitNorm rd) rd
            let costheta = if insideObj then costheta else -costheta
            let n1 = if insideObj then ri else 1
            let n2 = if insideObj then 1 else ri
            let pReflection = schlick n1 n2 costheta
            let u1 = rand x y ray bounces 5
            in if pReflection > u1
               then perfectReflect
               else trace (snell rd hitNorm n1 n2)
        let newRay = vec3.normalise newRay
        in (bounces+1, colour vec3.* mat.colour, newRay,
            hitPos vec3.+ (vec3.scale (10*epsilon) newRay), false)
  in if bounces != maxBounces then colour else col(0,0,0)

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
  let camPos = vec(0, 3, -8)
  let lookAt = vec(0, 2, 0)
  let filmCentre = camPos vec3.+ vec3.normalise(lookAt vec3.- camPos)
  let fov = 1
  let aspectRatio = f32.i32 width / f32.i32 height
  let filmPos = filmCentre vec3.+ vec(x*fov*aspectRatio, y*fov, 0)
  let rd = vec3.normalise(filmPos vec3.- camPos)
  let ro = camPos
  in gammaCorrect <| pixel x y rd ro

let bounds lower upper x : f32 = if x < lower then lower else (if x > upper then upper else x)

let packCol ({x=r, y=g, z=b} : col3) : [3]u8 = map (u8.f32 <-< bounds 0 255 <-< (* 255)) [r, g, b]

let canvas (y: i32) (x: i32) : [3]u8 =
  packCol <| (shader (-((f32.i32 y) / (f32.i32 height) - 0.5)) ((f32.i32 x) / (f32.i32 width) - 0.5))

entry main : [height][width][3]u8 =
  map (\x -> (map (canvas x) (iota width))) (iota height)
