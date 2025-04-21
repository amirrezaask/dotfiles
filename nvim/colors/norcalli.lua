local base16 = require("base16")
local RGB = base16.rgb

base16.theme
  .new("norcalli", {
    -- UI
    RGB.from_hex("#121b2b"), -- base00
    RGB.from_hex("#213554"), -- base01
    RGB.from_hex("#1d3872"), -- base02
    RGB.from_hex("#80b2d6"), -- base03
    RGB.from_hex("#3aa3e9"), -- base04
    RGB.from_hex("#abb2bf"), -- base05
    RGB.from_hex("#b6bdca"), -- base06
    RGB.from_hex("#c8ccd4"), -- base07

    -- Syntax
    RGB.from_hex("#f04c75"), -- base08
    RGB.from_hex("#d19a66"), -- base09
    RGB.from_hex("#e5c07b"), -- base0A
    RGB.from_hex("#98c379"), -- base0B
    RGB.from_hex("#56b6c2"), -- base0C
    RGB.from_hex("#01bfef"), -- base0D
    RGB.from_hex("#c678dd"), -- base0E
    RGB.from_hex("#be5046"), -- base0F
  })
  :apply()
