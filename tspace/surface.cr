module Ww
  # Sensors and appearances together are referred to as *surfaces*.
  abstract struct Surface
    # `Tbase`-unique integer that identifies a surface. Must only be used once
    # per `Tbase`, and then disposed forever.
    alias Id = UInt32
  end

  # Identifies a sensor.
  record Sensor < Surface, id : Id

  # Identifies an appearance.
  record Appearance < Surface, id : Id
end
