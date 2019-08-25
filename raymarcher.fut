import "utils"
import "material"
import "scene"

let slow : bool = false
let height : i32 = if slow then 400 else 300
let width : i32 = if slow then 600 else 300

let maxSteps : i32 = if slow then 200 else 100
let maxBounces : i32 = if slow then 50 else 20
let maxRays : i32 = if slow then 10000 else 2000
let epsilon : f32 = 0.01

type hit = #no_hit | #hit {hitPos: vec3, mat: material, insideObj: bool}

let getNorm sdf p : vec3 =
  let d = vec(fst(sdf(p vec3.+ vec(epsilon, 0, 0)))
             ,fst(sdf(p vec3.+ vec(0, epsilon, 0)))
             ,fst(sdf(p vec3.+ vec(0, 0, epsilon))))
  let n = d vec3.- vec(fst(sdf(p vec3.- vec(epsilon, 0, 0)))
                      ,fst(sdf(p vec3.- vec(0, epsilon, 0)))
                      ,fst(sdf(p vec3.- vec(0, 0, epsilon))))
  in vec3.normalise n

type wavelength = #red | #green | #blue
type ray = {rd: vec3, ro: vec3, wavelength: wavelength}

let march sdf (rd : vec3) (ro : vec3) : hit =
  let steps = 0
  let dist : f32 = -1
  let d : f32 = 1
  let mat = diffuse black
  let insideObj = false
  let (_, _, d, ro, mat, insideObj) : (i32, f32, f32, vec3, material, bool) =
    loop (steps, dist, d, ro, mat, insideObj) while (steps < maxSteps && (dist == -1 || d >= epsilon)) do
    let (d, mat) = sdf ro
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
let snell rd surfaceNorm n1 n2 : maybe vec3 =
  let normal = if vec3.dot rd surfaceNorm <= 0
               then surfaceNorm
               else vinvert surfaceNorm
  let r = n1 / n2
  let rootTerm = 1 - (r*r) * vec3.quadrance (vec3.cross normal rd)
  in if rootTerm < 0
     then #nothing
     else #just
          ((vec3.scale r (vec3.cross normal (vec3.cross (vinvert normal) rd)))
           vec3.- (vec3.scale (f32.sqrt rootTerm) normal))

-- Schlick's approximation of the Fresnel factor for calulating the
-- contribution ratio of reflected to refracted light
let schlick n1 n2 costheta : f32 =
  let r0 = ((n1 - n2) / (n1 + n2)) ** 2
  in r0 + (1 - r0) * ((1 - costheta) ** 5)

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

let sampleUniformHemisphere n x y ray bounce : vec3 =
  let v = sampleSphere x y ray bounce
  in normalTowards n v

type mat33 = {a: vec3, b: vec3, c: vec3}

let mulMat33 (m : mat33) (v : vec3) : vec3 =
  vec( v.x * m.a.x + v.y * m.b.x + v.z * m.c.x
     , v.x * m.a.y + v.y * m.b.y + v.z * m.c.y
     , v.x * m.a.z + v.y * m.b.z + v.z * m.c.z)

let transposeM33 (m : mat33) : mat33 =
  { a = vec(m.a.x, m.b.x, m.c.x)
  , b = vec(m.a.y, m.b.y, m.c.y)
  , c = vec(m.a.z, m.b.z, m.c.z)}

let sampleCosineHemisphere n x y ray bounce : vec3 =
  let u1 = rand x y ray bounce 0
  let u2 = rand x y ray bounce 10
  let r = f32.sqrt u1
  let theta = tau * u2
  let tangent' = vec3.cross n (vec(0,1,0))
  let tangent =
    if vec3.quadrance tangent' <= 0.001
    then vec(1,0,0)
    else vec3.normalise tangent'
  let bitangent = vec3.cross tangent n
  let tangentToWorld = {a = tangent, b = bitangent, c = n}
  let vTangentSpace
    = vec(r * f32.cos theta, r * f32.sin theta, f32.sqrt(1 - u1))
  in mulMat33 tangentToWorld vTangentSpace

let getRi (wavelength: wavelength) (ri : refractiveIndex) : f32 =
  match wavelength
  case #red -> ri.riRed
  case #green -> ri.riGreen
  case #blue -> ri.riBlue

let castRay sdf globalLight rayNum x y (ray : ray) : col3 =
  let bounces = 0
  let colour = col(1.0, 1.0, 1.0)
  let break = false
  let rd = ray.rd
  let ro = ray.ro
  let (bounces, colour, _, _, _) =
    loop (bounces, colour, rd, ro, break) while (bounces < maxBounces && !break) do
         match march sdf rd ro
         case #no_hit -> (bounces, colour vec3.* globalLight rd, rd, ro, true)
         case #hit {hitPos, mat, insideObj} ->
      if mat.light
      then (bounces, colour vec3.* mat.colour, vec(0,0,0), vec(0,0,0), true)
      else
        let hitNorm = getNorm sdf hitPos
        let hitNorm = if insideObj then vinvert hitNorm else hitNorm
        let perfectReflect = reflect rd hitNorm
        let scattered = sampleCosineHemisphere hitNorm x y rayNum bounces
        let reflected = lerp scattered perfectReflect (mat.reflectivity)
        let newRay =
          match mat.transparent
          case #nothing ->
            reflected
          case #just ri ->
            let ri = getRi ray.wavelength ri
            let costheta = -(vec3.dot hitNorm rd)
            let n1 = if insideObj then ri else 1
            let n2 = if insideObj then 1 else ri
            let u1 = rand x y rayNum bounces 5
            in match snell rd hitNorm n1 n2
               case #nothing -> perfectReflect
               case #just refract ->
                 let pReflection = schlick n1 n2 costheta
                 in if pReflection > u1
                    then perfectReflect
                    else refract
        let newRay = vec3.normalise newRay
        in (bounces+1, colour vec3.* mat.colour, newRay,
            hitPos vec3.+ (vec3.scale (10*epsilon) newRay), false)
  in if bounces != maxBounces then colour else col(0,0,0)

let getWavelengthColour (wavelength : wavelength) : col3 =
  match wavelength
  case #red -> col(1.0, 0.0, 0.0)
  case #green -> col(0.0, 1.0, 0.0)
  case #blue -> col(0.0, 0.0, 1.0)

let jitterRay sdf globalLight i x y rd ro =
  let u1 = rand x y i 0 2
  let u2 = rand x y i 0 3
  let u3 = rand x y i 0 11 * 3
  let wavelength = if u3 <= 1
                   then #red
                   else if u3 <= 2
                   then #green
                   else #blue
  --TODO: jitter position, not direction
  let rd = vec3.normalise <| rd vec3.+ vec(u1 / f32.i32 width, u2 / f32.i32 height, 0)
  let wavelengthColour = getWavelengthColour wavelength
  in wavelengthColour vec3.* castRay sdf globalLight i x y {rd, ro, wavelength}

let pixel sdf globalLight x y rd ro : col3 =
  let mr = f32.i32 maxRays
  in reduce (vec3.+) (col(0,0,0))
            (map (\i -> jitterRay sdf globalLight i x y rd ro)
                 (iota maxRays))
            vec3./ col(mr,mr,mr)

let gammaCorrect c =
  vec3.map (f32.** 0.45) c

let marcher sdf globalLight camPos lookAt (x: f32) (y:f32) : col3 =
  let filmCentre = camPos vec3.+ vec3.normalise(lookAt vec3.- camPos)
  let fov = 1
  let aspectRatio = f32.i32 width / f32.i32 height
  let filmPos = filmCentre vec3.+ vec(x*fov*aspectRatio, y*fov, 0)
  let rd = vec3.normalise(filmPos vec3.- camPos)
  let ro = camPos
  in gammaCorrect <| pixel sdf globalLight x y rd ro

let shader {sdf, globalLight, camera} (y: f32) (x: f32) : col3 =
  match (camera : camera)
  case #simple {camPos, lookAt} ->
    marcher sdf globalLight camPos lookAt x y

let packCol ({x=r, y=g, z=b} : col3) : [3]u8 = map (u8.f32 <-< clamp 0 255 <-< (* 255)) [r, g, b]

let canvas scene (y: i32) (x: i32) : [3]u8 =
  packCol <| (shader scene (-((f32.i32 y) / (f32.i32 height) - 0.5)) ((f32.i32 x) / (f32.i32 width) - 0.5))

let debug = false
let debugX : i32 = 50
let debugY : i32 = 152

let raymarcherDebug scene : [height][width][3]u8 =
  map (\y -> (map (\x -> if x == debugX && y == debugY
                         then canvas scene y x
                         else [0,255,0])
                  (iota width)))
      (iota height)

let raymarcher scene : [height][width][3]u8 =
  if debug
  then raymarcherDebug scene
  else map (\y -> (map (canvas scene y) (iota width))) (iota height)
