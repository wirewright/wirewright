module Ww::Console
  extend self

  alias Widget = MuBanner | WwBanner | InfoLog | NoteLog | ErrLog | CriticalLog

  defrecord WwBanner
  defrecord MuBanner
  defrecord NoteLog, message : String
  defrecord InfoLog, message : String
  defrecord ErrLog, message : String
  defrecord CriticalLog, message : String

  def display(io : IO, widget : MuBanner)
    io << <<-'BANNER'

        ############
       ##############
       ##############
       #####     ####
       #####     ####    Wirewright µsoma
        #############    https://github.com/wirewright/wirewright [iota]
         ###########
       #   ######   #
       ###        ###
       ###############
        ###############


    BANNER
  end

  def display(io : IO, widget : WwBanner)
    io << <<-'BANNER'

     ##############
     ##############
     #####    #####    Wirewright
     ####      ####    https://github.com/wirewright/wirewright [iota]
     #####    #####
      ############
        ########


    BANNER
  end

  def display(io : IO, widget : InfoLog)
    timestamp = Time.local.to_s("%F %T")

    Colorize.with.dark_gray.surround(io) do
      io << " LOG  " << timestamp << "  " << widget.message
      io.puts
    end
  end

  def display(io : IO, widget : NoteLog)
    timestamp = Time.local.to_s("%F %T")

    io << " NOTE  ".colorize.green.bold << timestamp << "  " << widget.message
    io.puts
  end

  def display(io : IO, widget : ErrLog)
    timestamp = Time.local.to_s("%F %T")

    io << " ERR  ".colorize.yellow.bold << timestamp << "  " << widget.message
    io.puts
  end

  def display(io : IO, widget : CriticalLog)
    timestamp = Time.local.to_s("%F %T")

    io << " CRITICAL  ".colorize.red.bold << timestamp << "  " << widget.message
    io.puts
  end
end
