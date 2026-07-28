alias Source = MLSource | CrSource

record MLSource, string : String do
  def self.regex : Regex
    /^\h*;;\h*\|@\h+[^\v]+(?:\v\h*;;[^\v]*)*$/m
  end
end

record CrSource, string : String do
  def self.regex : Regex
    /^\h*#\h*\|@\h+[^\v]+(?:\v\h*#[^\v]*)*$/m
  end
end

def articles(sources : Iterator(Source)) : Iterator(String)
  sources.flat_map do |source|
    source.string.scan(source.class.regex).map do |match|
      match.to_s
        .each_line
        .map(&.lstrip(' ').lchop('#').lchop(' '))
        .join('\n')
    end
  end
end

sources = Iterator.chain({
  Dir["runtime/**/*.wwml"].each
    .map { |path| File.read(path) }
    .map { |string| MLSource.new(string) },
  Dir["src/**/*.cr"].each
    .map { |path| File.read(path) }
    .map { |string| CrSource.new(string) },
})

articles = articles(sources)

File.open("#{__DIR__}/articles.txt", "w") do |io|
  io << '['
  articles.each do |article|
    article.dump(io)
    io << ','
  end
  io << ']'
  io.flush
end
