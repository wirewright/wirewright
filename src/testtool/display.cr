module Testtool
  extend self

  alias DisplayEntity = Banner |
                        Help |
                        TestSuccessPixel |
                        TestFailurePixel |
                        Status |
                        LogMessage |
                        WarnMessage |
                        ErrMessage |
                        Text |
                        Hr |
                        AssertionReportHeader |
                        ComparisonReportHeader |
                        ComplaintRef |
                        ComplaintList |
                        ComplaintAttachment

  defrecord Banner
  defrecord Help

  defrecord TestSuccessPixel, color : Pigment::RGBA
  defrecord TestFailurePixel

  defrecord Status, successes : Int32, failures : Int32, mmt : Mmt

  defrecord LogMessage, message : String
  defrecord WarnMessage, message : String
  defrecord ErrMessage, message : String
  defrecord Text, text : String
  defrecord Hr

  defrecord AssertionReportHeader, term : Term
  defrecord ComparisonReportHeader, term : Term

  defrecord ComplaintRef, ref : String
  defrecord ComplaintList, complaints : Array(Complaint)
  defrecord ComplaintAttachment, text : String

  @@display_running = Atomic(Bool).new(false)
  @@display_chan = Channel(DisplayEntity).new

  private def ensure_display_running! : Nil
    return if @@display_running.swap(true)

    spawn(name: "testtool display") do
      display = Display.new
      loop do
        msg = @@display_chan.receive
        display.show(msg)
      end
    end
  end

  private class Display
    def initialize
      @rng = Random::PCG32.new
    end

    # Reference: https://www.asciiart.eu/image-to-ascii
    def show(entity : Banner)
      STDERR << <<-'BANNER'

       ##############
       ##############
       #####    #####    Wirewright
       ####      ####    https://github.com/wirewright/wirewright [iota]
       #####    #####
        ############
          ########


      BANNER
    end

    def show(entity : Help)
      puts HELP
    end

    def show(entity : LogMessage) : Nil
      Colorize.with.dark_gray.surround(STDERR) do
        instant = Time.local.to_s("%F %T")

        STDERR << " LOG  " << instant << "  "
        STDERR.puts entity.message
      end
    end

    def show(entity : WarnMessage) : Nil
      instant = Time.local.to_s("%F %T")

      STDERR << "WARN  ".colorize.yellow << instant << "  "
      STDERR.puts entity.message
    end

    def show(entity : ErrMessage) : Nil
      instant = Time.local.to_s("%F %T")

      STDERR << " ERR  ".colorize.red.bold << instant << "  "
      STDERR.puts entity.message
    end

    def show(entity : Text) : Nil
      STDERR.puts entity.text
    end

    def show(entity : Hr) : Nil
      STDERR.puts
    end

    def show(entity : TestSuccessPixel)
      # NOTE: I find it very tiring/headache-y when the same glyph (e.g. `.`) repeats
      # over and over in peripheral vision, so let's add some texture. Also, color is
      # much easier to distinguish with texture.
      glyph = {
        "⠁", "⠂", "⠃", "⠄", "⠅", "⠆", "⠇", "⠈", "⠉", "⠊", "⠋", "⠌", "⠍", "⠎", "⠏",
        "⠐", "⠑", "⠒", "⠓", "⠔", "⠕", "⠖", "⠗", "⠘", "⠙", "⠚", "⠛", "⠜", "⠝", "⠞", "⠟",
        "⠠", "⠡", "⠢", "⠣", "⠤", "⠥", "⠦", "⠧", "⠨", "⠩", "⠪", "⠫", "⠬", "⠭", "⠮", "⠯",
        "⠰", "⠱", "⠲", "⠳", "⠴", "⠵", "⠶", "⠷", "⠸", "⠹", "⠺", "⠻", "⠼", "⠽", "⠾", "⠿",
      }.sample(@rng)

      STDERR.print glyph.colorize.fore(*entity.color.rgb8)
    end

    def show(entity : TestFailurePixel)
      STDERR.print "X".colorize.red
    end

    def show(entity : Status) : Nil
      if entity.failures.zero?
        STDERR.puts "▊ #{entity.successes} assertion(s) succeeded.".colorize.light_green.bold
      else
        STDERR.puts "  #{entity.successes} assertion(s) succeeded."
        STDERR.puts "▊ #{entity.failures} assertion(s) failed.".colorize.red.bold
      end

      STDERR.print " " # ?!
      STDERR.puts entity.mmt
    end

    def show(entity : AssertionReportHeader) : Nil
      text = ML.display(entity.term, endl: false)

      decorated = wrap(text, maxw: 80)
        .each_line(chomp: true)
        .map { |line| " │ ".colorize.dark_gray.to_s + line } # ?!
        .join('\n')

      STDERR.puts " ASSERTION".colorize.bold # ?!
      STDERR.puts decorated
    end

    def show(entity : ComparisonReportHeader) : Nil
      text = ML.display(entity.term, endl: false)

      decorated = wrap(text, maxw: 80)
        .each_line(chomp: true)
        .map { |line| " │ ".colorize.dark_gray.to_s + line } # ?!
        .join('\n')

      STDERR.puts " COMPARISON".colorize.bold # ?!
      STDERR.puts decorated
    end

    def show(entity : ComplaintRef) : Nil
      STDERR.puts "▍#{entity.ref.colorize.underline}"
    end

    def show(entity : ComplaintList) : Nil
      STDERR.puts " COMPLAINTS".colorize.bold

      entity.complaints.each do |complaint|
        STDERR.puts "   #{complaint.title}"

        complaint.attachments.each do |label, attachment|
          STDERR.puts "     ⦾ #{label.capitalize}"

          show(ComplaintAttachment, attachment)
        end
      end
    end

    def show(entity : ComplaintAttachment) : Nil
      decorated = wrap(entity.text, maxw: 80)
        .each_line(chomp: true)
        .map { |line| "     │ ".colorize.dark_gray.to_s + line } # ?!
        .join('\n')

      STDERR.puts decorated
    end

    def show(entity : ComplaintAttachment.class, attachment : Term) : Nil
      text = ML.display(attachment, endl: false)

      show(ComplaintAttachment.new(text))
    end

    def show(entity : ComplaintAttachment.class, attachment : ML::SyntaxError) : Nil
      text = attachment.humanize(styled: false)

      show(ComplaintAttachment.new(text))
    end

    def show(entity : ComplaintAttachment.class, attachment : Exception) : Nil
      text = attachment.inspect_with_backtrace.chomp

      show(ComplaintAttachment.new(text))
    end
  end

  def display(entity : DisplayEntity) : Nil
    ensure_display_running!

    @@display_chan << entity
  end

  # Displays a log message.
  def log(message : String) : Nil
    display(LogMessage.new(message))
  end

  # Displays a warning message.
  def warn(message : String) : Nil
    display(WarnMessage.new(message))
  end

  # Displays a warning message pointing at *path* and *srcmap*'s root.
  def warn(message : String, path : Path, srcmap : ML::SrcMap) : Nil
    unless text = srcmap[Tpath[]]?
      return warn(message, path)
    end

    _, line, column = ML::SyntaxError.lookaround(text)
    warn("#{message} (#{Ww.normalize(path)}:#{line}:#{column})")
  end

  # Displays a warning message pointing at *path*.
  def warn(message : String, path : Path)
    warn("#{message} (#{Ww.normalize(path)})")
  end

  # Displays an error message.
  def err(message : String) : Nil
    display(ErrMessage.new(message))
  end

  # Displays some *text*.
  def dump(text : String) : Nil
    display(Text.new(text))
  end

  # Displays a horizontal separator.
  def hr : Nil
    display(Hr.new)
  end

  # Displays the Wirewright banner.
  def banner : Nil
    display(Banner.new)
  end

  # Displays the help message.
  def help : Nil
    display(Help.new)
  end
end
