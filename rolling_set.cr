class RollingSet(T, N)
  def initialize
    @ring = StaticArray(T?, N).new { }
    @cursor = 0
  end

  def add?(element : T) : Bool
    if @cursor >= @ring.size
      @cursor = 0
    end
    return false if @ring.any?(element)
    @ring[@cursor] = element
    @cursor &+= 1
    true
  end
end
