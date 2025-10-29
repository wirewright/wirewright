# Primitive support for CSS or CSS-like colors (color strings).
#
# Reference: https://drafts.csswg.org/css-color.
module Ww::CSSColor
  extend self

  # :nodoc:
  NAMED = {
    aliceblue:            {240u8, 248u8, 255u8, 255u8},
    antiquewhite:         {250u8, 235u8, 215u8, 255u8},
    aqua:                 {0u8, 255u8, 255u8, 255u8},
    aquamarine:           {127u8, 255u8, 212u8, 255u8},
    azure:                {240u8, 255u8, 255u8, 255u8},
    beige:                {245u8, 245u8, 220u8, 255u8},
    bisque:               {255u8, 228u8, 196u8, 255u8},
    black:                {0u8, 0u8, 0u8, 255u8},
    blanchedalmond:       {255u8, 235u8, 205u8, 255u8},
    blue:                 {0u8, 0u8, 255u8, 255u8},
    blueviolet:           {138u8, 43u8, 226u8, 255u8},
    brown:                {165u8, 42u8, 42u8, 255u8},
    burlywood:            {222u8, 184u8, 135u8, 255u8},
    cadetblue:            {95u8, 158u8, 160u8, 255u8},
    chartreuse:           {127u8, 255u8, 0u8, 255u8},
    chocolate:            {210u8, 105u8, 30u8, 255u8},
    coral:                {255u8, 127u8, 80u8, 255u8},
    cornflowerblue:       {100u8, 149u8, 237u8, 255u8},
    cornsilk:             {255u8, 248u8, 220u8, 255u8},
    crimson:              {220u8, 20u8, 60u8, 255u8},
    cyan:                 {0u8, 255u8, 255u8, 255u8},
    darkblue:             {0u8, 0u8, 139u8, 255u8},
    darkcyan:             {0u8, 139u8, 139u8, 255u8},
    darkgoldenrod:        {184u8, 134u8, 11u8, 255u8},
    darkgray:             {169u8, 169u8, 169u8, 255u8},
    darkgreen:            {0u8, 100u8, 0u8, 255u8},
    darkgrey:             {169u8, 169u8, 169u8, 255u8},
    darkkhaki:            {189u8, 183u8, 107u8, 255u8},
    darkmagenta:          {139u8, 0u8, 139u8, 255u8},
    darkolivegreen:       {85u8, 107u8, 47u8, 255u8},
    darkorange:           {255u8, 140u8, 0u8, 255u8},
    darkorchid:           {153u8, 50u8, 204u8, 255u8},
    darkred:              {139u8, 0u8, 0u8, 255u8},
    darksalmon:           {233u8, 150u8, 122u8, 255u8},
    darkseagreen:         {143u8, 188u8, 143u8, 255u8},
    darkslateblue:        {72u8, 61u8, 139u8, 255u8},
    darkslategray:        {47u8, 79u8, 79u8, 255u8},
    darkslategrey:        {47u8, 79u8, 79u8, 255u8},
    darkturquoise:        {0u8, 206u8, 209u8, 255u8},
    darkviolet:           {148u8, 0u8, 211u8, 255u8},
    deeppink:             {255u8, 20u8, 147u8, 255u8},
    deepskyblue:          {0u8, 191u8, 255u8, 255u8},
    dimgray:              {105u8, 105u8, 105u8, 255u8},
    dimgrey:              {105u8, 105u8, 105u8, 255u8},
    dodgerblue:           {30u8, 144u8, 255u8, 255u8},
    firebrick:            {178u8, 34u8, 34u8, 255u8},
    floralwhite:          {255u8, 250u8, 240u8, 255u8},
    forestgreen:          {34u8, 139u8, 34u8, 255u8},
    fuchsia:              {255u8, 0u8, 255u8, 255u8},
    gainsboro:            {220u8, 220u8, 220u8, 255u8},
    ghostwhite:           {248u8, 248u8, 255u8, 255u8},
    gold:                 {255u8, 215u8, 0u8, 255u8},
    goldenrod:            {218u8, 165u8, 32u8, 255u8},
    gray:                 {128u8, 128u8, 128u8, 255u8},
    green:                {0u8, 128u8, 0u8, 255u8},
    greenyellow:          {173u8, 255u8, 47u8, 255u8},
    grey:                 {128u8, 128u8, 128u8, 255u8},
    honeydew:             {240u8, 255u8, 240u8, 255u8},
    hotpink:              {255u8, 105u8, 180u8, 255u8},
    indianred:            {205u8, 92u8, 92u8, 255u8},
    indigo:               {75u8, 0u8, 130u8, 255u8},
    ivory:                {255u8, 255u8, 240u8, 255u8},
    khaki:                {240u8, 230u8, 140u8, 255u8},
    lavender:             {230u8, 230u8, 250u8, 255u8},
    lavenderblush:        {255u8, 240u8, 245u8, 255u8},
    lawngreen:            {124u8, 252u8, 0u8, 255u8},
    lemonchiffon:         {255u8, 250u8, 205u8, 255u8},
    lightblue:            {173u8, 216u8, 230u8, 255u8},
    lightcoral:           {240u8, 128u8, 128u8, 255u8},
    lightcyan:            {224u8, 255u8, 255u8, 255u8},
    lightgoldenrodyellow: {250u8, 250u8, 210u8, 255u8},
    lightgray:            {211u8, 211u8, 211u8, 255u8},
    lightgreen:           {144u8, 238u8, 144u8, 255u8},
    lightgrey:            {211u8, 211u8, 211u8, 255u8},
    lightpink:            {255u8, 182u8, 193u8, 255u8},
    lightsalmon:          {255u8, 160u8, 122u8, 255u8},
    lightseagreen:        {32u8, 178u8, 170u8, 255u8},
    lightskyblue:         {135u8, 206u8, 250u8, 255u8},
    lightslategray:       {119u8, 136u8, 153u8, 255u8},
    lightslategrey:       {119u8, 136u8, 153u8, 255u8},
    lightsteelblue:       {176u8, 196u8, 222u8, 255u8},
    lightyellow:          {255u8, 255u8, 224u8, 255u8},
    lime:                 {0u8, 255u8, 0u8, 255u8},
    limegreen:            {50u8, 205u8, 50u8, 255u8},
    linen:                {250u8, 240u8, 230u8, 255u8},
    magenta:              {255u8, 0u8, 255u8, 255u8},
    maroon:               {128u8, 0u8, 0u8, 255u8},
    mediumaquamarine:     {102u8, 205u8, 170u8, 255u8},
    mediumblue:           {0u8, 0u8, 205u8, 255u8},
    mediumorchid:         {186u8, 85u8, 211u8, 255u8},
    mediumpurple:         {147u8, 112u8, 219u8, 255u8},
    mediumseagreen:       {60u8, 179u8, 113u8, 255u8},
    mediumslateblue:      {123u8, 104u8, 238u8, 255u8},
    mediumspringgreen:    {0u8, 250u8, 154u8, 255u8},
    mediumturquoise:      {72u8, 209u8, 204u8, 255u8},
    mediumvioletred:      {199u8, 21u8, 133u8, 255u8},
    midnightblue:         {25u8, 25u8, 112u8, 255u8},
    mintcream:            {245u8, 255u8, 250u8, 255u8},
    mistyrose:            {255u8, 228u8, 225u8, 255u8},
    moccasin:             {255u8, 228u8, 181u8, 255u8},
    navajowhite:          {255u8, 222u8, 173u8, 255u8},
    navy:                 {0u8, 0u8, 128u8, 255u8},
    oldlace:              {253u8, 245u8, 230u8, 255u8},
    olive:                {128u8, 128u8, 0u8, 255u8},
    olivedrab:            {107u8, 142u8, 35u8, 255u8},
    orange:               {255u8, 165u8, 0u8, 255u8},
    orangered:            {255u8, 69u8, 0u8, 255u8},
    orchid:               {218u8, 112u8, 214u8, 255u8},
    palegoldenrod:        {238u8, 232u8, 170u8, 255u8},
    palegreen:            {152u8, 251u8, 152u8, 255u8},
    paleturquoise:        {175u8, 238u8, 238u8, 255u8},
    palevioletred:        {219u8, 112u8, 147u8, 255u8},
    papayawhip:           {255u8, 239u8, 213u8, 255u8},
    peachpuff:            {255u8, 218u8, 185u8, 255u8},
    peru:                 {205u8, 133u8, 63u8, 255u8},
    pink:                 {255u8, 192u8, 203u8, 255u8},
    plum:                 {221u8, 160u8, 221u8, 255u8},
    powderblue:           {176u8, 224u8, 230u8, 255u8},
    purple:               {128u8, 0u8, 128u8, 255u8},
    rebeccapurple:        {102u8, 51u8, 153u8, 255u8},
    red:                  {255u8, 0u8, 0u8, 255u8},
    rosybrown:            {188u8, 143u8, 143u8, 255u8},
    royalblue:            {65u8, 105u8, 225u8, 255u8},
    saddlebrown:          {139u8, 69u8, 19u8, 255u8},
    salmon:               {250u8, 128u8, 114u8, 255u8},
    sandybrown:           {244u8, 164u8, 96u8, 255u8},
    seagreen:             {46u8, 139u8, 87u8, 255u8},
    seashell:             {255u8, 245u8, 238u8, 255u8},
    sienna:               {160u8, 82u8, 45u8, 255u8},
    silver:               {192u8, 192u8, 192u8, 255u8},
    skyblue:              {135u8, 206u8, 235u8, 255u8},
    slateblue:            {106u8, 90u8, 205u8, 255u8},
    slategray:            {112u8, 128u8, 144u8, 255u8},
    slategrey:            {112u8, 128u8, 144u8, 255u8},
    snow:                 {255u8, 250u8, 250u8, 255u8},
    springgreen:          {0u8, 255u8, 127u8, 255u8},
    steelblue:            {70u8, 130u8, 180u8, 255u8},
    tan:                  {210u8, 180u8, 140u8, 255u8},
    teal:                 {0u8, 128u8, 128u8, 255u8},
    thistle:              {216u8, 191u8, 216u8, 255u8},
    tomato:               {255u8, 99u8, 71u8, 255u8},
    turquoise:            {64u8, 224u8, 208u8, 255u8},
    violet:               {238u8, 130u8, 238u8, 255u8},
    wheat:                {245u8, 222u8, 179u8, 255u8},
    white:                {255u8, 255u8, 255u8, 255u8},
    whitesmoke:           {245u8, 245u8, 245u8, 255u8},
    yellow:               {255u8, 255u8, 0u8, 255u8},
    yellowgreen:          {154u8, 205u8, 50u8, 255u8},
    transparent:          {0u8, 0u8, 0u8, 0u8},
  }

  # Returns RGBA for a named color with the given *name*, or `nil` if
  # not found.
  #
  # Reference: https://drafts.csswg.org/css-color/#named-color.
  def named?(name : String) : {UInt8, UInt8, UInt8, UInt8}?
    NAMED[name]?
  end

  private def hexcolor?(r : Rtk::R)
    Rtk.skip(r, " ")
    return unless Rtk.ahead?(r, "#")

    Rtk.forward(r)

    # Try to read all the way up to RRGGBBAA.
    d0 = Rtk.hexdigit?(r)
    d1 = d0 && Rtk.hexdigit?(r)
    d2 = d1 && Rtk.hexdigit?(r)
    d3 = d2 && Rtk.hexdigit?(r)
    d4 = d3 && Rtk.hexdigit?(r)
    d5 = d4 && Rtk.hexdigit?(r)
    d6 = d5 && Rtk.hexdigit?(r)
    d7 = d6 && Rtk.hexdigit?(r)

    Rtk.skip(r, " ")
    return unless Rtk.at_end?(r)

    if d0 && d1 && d2 && d4.nil?
      r = (d0 << 4 | d0).to_u8
      g = (d1 << 4 | d1).to_u8
      b = (d2 << 4 | d2).to_u8
      a = d3 ? (d3 << 4 | d3).to_u8 : 255u8
      return r, g, b, a
    end

    if d0 && d1 && d2 && d3 && d4 && d5
      r = (d0 << 4 | d1).to_u8
      g = (d2 << 4 | d3).to_u8
      b = (d4 << 4 | d5).to_u8
      if d6.nil?
        return r, g, b, 255u8
      end
      if d7
        return r, g, b, (d6 << 4 | d7).to_u8
      end
    end
  end

  # Attempts to parse *string* as a hex color. Returns RGBA on success,
  # `nil` on failure.
  #
  # In theory, the reference is: https://drafts.csswg.org/css-color/#hex-notation.
  # In practice, I don't know how much we (need to) adhere to it here.
  def hexcolor?(string : String) : {UInt8, UInt8, UInt8, UInt8}?
    reader = Char::Reader.new(string)

    hexcolor?(pointerof(reader))
  end
end
