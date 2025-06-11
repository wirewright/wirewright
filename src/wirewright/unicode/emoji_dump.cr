# Dump emojis from emoji.json to emoji.txt
#
# emoji.json source: https://github.com/github/gemoji/blob/0eca75db9301421efc8710baf7a7576793ae452a/db/emoji.json
#
# Each row in emoji.txt is formatted like so:
#
#    <alias of emoji>;<emoji>\n
#
# The same emoji can correspond to different aliases.

require "json"

names = [] of String
emoji = nil

File.open("#{__DIR__}/emoji.json", "r") do |src|
  File.open("#{__DIR__}/emoji.txt", "w") do |dst|
    pull = JSON::PullParser.new(src)
    pull.read_array do
      pull.read_object do |key|
        case key
        when "emoji"
          emoji = pull.read_string
        when "aliases"
          pull.read_array do
            names << pull.read_string
          end
        else
          pull.skip
        end
      end

      unless emoji
        raise "missing emoji"
      end

      names.each do |name|
        dst << name << ";" << emoji << "\n"
      end

      names.clear
    end
  end
end
