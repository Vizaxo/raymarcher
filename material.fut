import "utils"

type material =
  { colour: col3
  , reflectivity: f32
  , light: bool
  , transparent: maybe f32}

let diffuse colour : material
  = {colour, light=false, reflectivity=0.2, transparent=#nothing}
let specular colour : material
  = {colour, light=false, reflectivity=1.0, transparent=#nothing}
let light colour : material
  = {colour, light=true, reflectivity=0.2, transparent=#nothing}
let mirror colour : material
  = {colour, light=false, reflectivity=1.0, transparent=#nothing}
let glass : material
  = {colour=col(1.0, 1.0, 1.0), light=false, reflectivity=1.0
     , transparent=#just 2}
let black = col(0.0, 0.0, 0.0)
let white = col(0.5, 0.5, 0.5)
let blue = col(0.05, 0.05, 0.3)
let red = col(1.0, 0.1, 0.1)
