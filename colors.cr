require "./wirewright"
require "./oklch"

module Colors
  @@table = {} of Term => {UInt8, UInt8, UInt8}
  @@lock = Mutex.new

  # Parse and initialize the colors table on boot.
  @@lock.synchronize do
    colors = ML.terms File.read("#{__DIR__}/colors.wwml")
    colors.each_entry do |color, spec|
      Term.case(spec) do
        matchpi %{(oklch l←(%number 0 <= _ <= 1) c←(%number 0 <= _ <= 1) h←(%number 0 <= _ <= 360))} do
          # 'colors' is a dict, there will be no duplication.
          @@table[color] = Oklch.to_rgb((l.to(Float64)*100).clamp(0.0..100.0), c.to(Float64), h.to(Float64))
        end

        otherwise do
          raise "invalid color spec for '#{color}': #{spec}"
        end
      end
    end
  end

  def self.rgb?(name : Term) : {UInt8, UInt8, UInt8}?
    @@lock.synchronize { @@table[name]? }
  end
end
