module Testtool
  extend self

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

  # Reference: https://www.asciiart.eu/image-to-ascii
  def display(entity : Banner)
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

  def display(entity : Help)
    puts <<-'HELP'
    SYNOPSIS

    `testtool` lets you run the Wirewright test suite.

    USAGE

      testtool [OPTIONS]

    OPTIONS

      +<tag>
        Focus topics tagged with <tag>.

      -<tag>
        Ignore topics tagged with <tag>.

      --index /path/to/index.wwml
        Changes the path to tests index.
        Default: tests/index.wwml.

      --stats
        Writes statistics (CPU and memory usage for each assertion)
        to /tmp/ww-testtool.stats.csv.

      --stats /path/to/stats.csv
        Writes statistics (CPU and memory usage for each assertion)
        to the provided path.

      --interactive, -i
        Go through failures (if any) one-by-one instead of printing
        them all at once.

      --help, -h
        Prints this message.

    EXAMPLE

      $ testtool --stats stats.csv -ufold -long
      # Runs all tests except ufold and long(-running ones).
      # Writes statistics to stats.csv.
    HELP
  end

  def display(entity : LogMessage) : Nil
    Colorize.with.dark_gray.surround(STDERR) do
      instant = Time.local.to_s("%F %T")

      STDERR << " LOG  " << instant << "  "
      STDERR.puts entity.message
    end
  end

  def display(entity : WarnMessage) : Nil
    instant = Time.local.to_s("%F %T")

    STDERR << "WARN  ".colorize.yellow << instant << "  "
    STDERR.puts entity.message
  end

  def display(entity : ErrMessage) : Nil
    instant = Time.local.to_s("%F %T")

    STDERR << " ERR  ".colorize.red.bold << instant << "  "
    STDERR.puts entity.message
  end

  def display(entity : Text) : Nil
    STDERR.puts entity.text
  end

  def display(entity : Hr) : Nil
    STDERR.puts
  end

  def display(entity : TestSuccessPixel)
    # NOTE: I find it very tiring/headache-y when the same glyph (e.g. `.`) repeats
    # over and over in peripheral vision, so let's add some texture.
    glyph = {",", "'", "`", ".", "\"", ";"}.sample

    STDERR.print glyph.colorize.fore(*entity.color.rgb8)
  end

  def display(entity : TestFailurePixel)
    STDERR.print "X".colorize.red
  end

  def display(entity : Status) : Nil
    if entity.failures.zero?
      STDERR.puts "▊ #{entity.successes} assertion(s) succeeded.".colorize.light_green.bold
    else
      STDERR.puts "  #{entity.successes} assertion(s) succeeded."
      STDERR.puts "▊ #{entity.failures} assertion(s) failed.".colorize.red.bold
    end

    STDERR.print " " # ?!
    STDERR.puts entity.mmt
  end

  def display(entity : AssertionReportHeader) : Nil
    text = ML.display(entity.term, endl: false)

    decorated = wrap(text, maxw: 80)
      .each_line(chomp: true)
      .map { |line| " │ ".colorize.dark_gray.to_s + line } # ?!
      .join('\n')

    STDERR.puts " ASSERTION".colorize.bold # ?!
    STDERR.puts decorated
  end

  def display(entity : ComparisonReportHeader) : Nil
    text = ML.display(entity.term, endl: false)

    decorated = wrap(text, maxw: 80)
      .each_line(chomp: true)
      .map { |line| " │ ".colorize.dark_gray.to_s + line } # ?!
      .join('\n')

    STDERR.puts " COMPARISON".colorize.bold # ?!
    STDERR.puts decorated
  end

  def display(entity : ComplaintRef) : Nil
    STDERR.puts "▍#{entity.ref.colorize.underline}"
  end

  def display(entity : ComplaintList) : Nil
    STDERR.puts " COMPLAINTS".colorize.bold

    entity.complaints.each do |complaint|
      STDERR.puts "   #{complaint.title}"

      complaint.attachments.each do |label, attachment|
        STDERR.puts "     ⦾ #{label.capitalize}"

        display(ComplaintAttachment, attachment)
      end
    end
  end

  def display(entity : ComplaintAttachment) : Nil
    decorated = wrap(entity.text, maxw: 80)
      .each_line(chomp: true)
      .map { |line| "     │ ".colorize.dark_gray.to_s + line } # ?!
      .join('\n')

    STDERR.puts decorated
  end

  def display(entity : ComplaintAttachment.class, attachment : Term) : Nil
    text = ML.display(attachment, endl: false)

    display(ComplaintAttachment.new(text))
  end

  def display(entity : ComplaintAttachment.class, attachment : ML::SyntaxError) : Nil
    text = attachment.humanize(styled: false)

    display(ComplaintAttachment.new(text))
  end

  def display(entity : ComplaintAttachment.class, attachment : Exception) : Nil
    text = attachment.inspect_with_backtrace.chomp

    display(ComplaintAttachment.new(text))
  end

  # Shorthands.

  # Displays a log message.
  def log(message : String) : Nil
    display(LogMessage.new(message))
  end

  # Displays a warning message.
  def warn(message : String) : Nil
    display(WarnMessage.new(message))
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
