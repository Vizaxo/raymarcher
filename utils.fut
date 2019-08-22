import "lib/github.com/athas/vector/vspace"

let tau = 2 * f32.pi

module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector
type col3 = vec3.vector

let vec (x, y, z) : vec3 = {x, y, z}
let col (r, g, b) : col3 = {x=r, y=g, z=b}

type maybe 't = #nothing | #just t

let fst (a, _) = a
let snd (_, b) = b

let lerp a b x = (vec3.scale (1 - x) a) vec3.+ (vec3.scale x b)

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

let clamp lower upper x : f32 = if x < lower then lower else (if x > upper then upper else x)

let fmod (x : f32) (y : f32) : f32 =
  let res = x / y
  in (res - f32.floor res) * y
