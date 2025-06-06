module Ww::Soma::DwUIR
  # A picture is a draw order-sorted collection of `DrawCommand`s.
  class Picture
    # Returns the bounding box of all `DrawCommand#tfbounds` in this picture.
    getter tfbounds : Rect = Rect.empty

    def initialize
      @sequence = [] of DrawCommand
      @population = Set(DrawCommand).new
      @finished = false
    end

    private def assert_finished : Nil
      unless @finished
        raise "expected a finished picture"
      end
    end

    private def assert_unfinished : Nil
      if @finished
        raise "expected an unfinished picture"
      end
    end

    # Appends a draw command *command* to this picture.
    def <<(command : DrawCommand) : Nil
      assert_unfinished

      @sequence << command

      register(command)
    end

    private def register(command : DrawShape) : Nil
      @population << command
    end

    private def register(command : DrawComposite) : Nil
      @population << command

      command.picture.each_command do |subcommand|
        register(subcommand)
      end
    end

    # Finishes this picture. You must call this method before iterating over
    # the commands using `each_command` or assessing `damage`.
    def finish : Nil
      assert_unfinished

      opaque = true

      @sequence.unstable_sort_by!(&.ord)
      @sequence.each do |command|
        @tfbounds = @tfbounds.max(command.tfbounds)
      end

      @finished = true
    end

    # Yields each draw command in this picture. The commands are yielded
    # in the order that you should draw them in.
    #
    # You must first `finish` the picture before calling this method.
    def each_command(& : DrawCommand ->) : Nil
      assert_finished

      @sequence.each { |command| yield command }
    end

    # Assesses the damage and yields damage rects for the transition between
    # this picture, assumed to be the newer one; and an *older* picture.
    #
    # TODO: The current algorithm is fairly contagious by itself. It helps only
    # in the "best-of-the-best" cases. For an example of how it fails: if we have
    # a background rect and something on top of it is damaged, the background rect
    # will be damaged; and everything on top of it, and so on. The next moment,
    # the entire part of the screen is damaged and all this work that we're doing
    # here was for nothing. It *does* make sense from a repainting point of view;
    # and I'm fine with keeping this behavior. The compositor's speed is OK and
    # we're not rasterizing anything; just re-compositing. But still, perhaps
    # having a way for the user to control this somehow would be nice.
    def damage(older : Picture, & : Rect ->) : Nil
      assert_finished

      intact = Deque(DrawCommand).new
      damaged = Deque(DrawCommand).new

      @population.each do |command|
        if command.in?(older.@population)
          intact << command
        else
          damaged << command
        end
      end

      older.@population.each do |command|
        next if command.in?(@population)

        damaged << command
      end

      while command0 = intact.shift?
        damaged.each do |command1|
          # Skip if damage does not touch intact command.
          next if (command1.dmgbounds & command0.dmgbounds).empty?

          damaged << command0

          break
        end
      end

      damaged.each do |command|
        yield command.dmgbounds.ceil
      end
    end

    def_equals_and_hash @sequence
  end
end
