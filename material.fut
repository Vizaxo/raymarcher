import "utils"

type refractiveIndex = {riRed: f32, riGreen: f32, riBlue: f32}

type material =
  { colour: col3
  , reflectivity: f32
  , light: bool
  , transparent: maybe refractiveIndex}

let diffuse colour : material
  = {colour, light=false, reflectivity=0.0, transparent=#nothing}
let specular colour : material
  = {colour, light=false, reflectivity=1.0, transparent=#nothing}
let light colour : material
  = {colour, light=true, reflectivity=0.2, transparent=#nothing}
let mirror colour : material
  = {colour, light=false, reflectivity=1.0, transparent=#nothing}
let glass : material
  = {colour=col(1.0, 1.0, 1.0), light=false, reflectivity=1.0
     , transparent=#just {riRed=1.40, riGreen=1.50, riBlue=1.60}}
let transparent colour (ri : refractiveIndex) : material
  = {colour, light=false, reflectivity=1.0
     , transparent=#just ri}
let black = col(0.0, 0.0, 0.0)
let white = col(0.5, 0.5, 0.5)
let blue = col(0.05, 0.05, 0.3)
let red = col(1.0, 0.1, 0.1)
