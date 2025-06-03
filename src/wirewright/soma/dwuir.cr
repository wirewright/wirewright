module Ww::Soma::DwUIR
  # TODO: move!!!
  alias TextSelectionRange = Range(Int32, Int32)?
end

require "./dwuir/wrap_token"
require "./dwuir/text_element"
require "./dwuir/text_command"
require "./dwuir/text_drawable"
require "./dwuir/walk"
