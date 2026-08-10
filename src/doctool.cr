require "ecr"
require "markd"
require "./wirewright"

module Doctool
  extend self
  include Ww

  Log = ::Log.for(self)

  DOCS = {{read_file("#{__DIR__}/doctool/articles.txt").id}}

  # Reference: https://github.com/crystal-lang/crystal/blob/13591c413f953a15d7a40a105fa7e46d1474c25c/src/compiler/crystal/tools/doc/markd_doc_renderer.cr#L57
  class Renderer < Markd::HTMLRenderer
    def code_body(node : Markd::Node)
      if in_link?(node)
        return output(node.text)
      end

      unless node.text =~ /^[\w°\-]+(?:\.[\w°\-]+)+$/
        return output(node.text)
      end

      href = $0.gsub('.', '/')

      literal(%(<a href="/#{href}/">#{node.text}</a>))
    end

    private def in_link?(node)
      return false unless parent = node.parent?
      return true if parent.type.link?

      in_link?(parent)
    end
  end

  def md2html(md : String) : String
    options = Markd::Options.new(smart: true, safe: true, autolink: true, gfm: true)
    document = Markd::Parser.parse(md, options)
    renderer = Renderer.new(options)
    renderer.render(document, formatter: nil)
  end

  alias Composition = Array(Component)
  alias Component = Ref | Pattern | Summary | Key | Block | PatternList | Example

  defcase Ref, parts : Array(String)
  defcase Pattern, specificity : M1::Specificity, op : M1::Op::Any
  defcase PatternList, source : String
  defcase Example, body : String

  defcase Key, capture : Term, ref : Ref?, body : String do
    def inspect(io)
      io << "Key(" << capture << ", ref=" << ref << ", body=...)"
    end
  end

  defcase Block, body : String do
    def inspect(io)
      io << "Block(body=...)"
    end
  end

  defcase Summary, body : String

  def composition(article : String) : Composition
    components = [] of Component

    # Find all `|@`s and turn them into `route` components.
    article.scan(/^\h*\|@\h+([^\v]+)$/m) do |(_, name)|
      components << Ref.new(name.split('.'))
    end

    # Find all `|@pattern`s and turn them into `pattern` components.
    article.scan(/^\h*\|@pattern\h*\v((?:(?!\v)[^\v]*(?:\v|\z))+)/m) do |(_, ml)|
      components << PatternList.new(ml)

      terms = ML.terms(ml)
      terms.items.each do |term|
        normp = M1.normal(term)
        specificity = M1.specificity(normp)
        op = M1.operator(normp)
        components << Pattern.new(specificity, op)
      end
    end

    # Find all `|@key`s with ref and turn them into `key` components.
    article.scan(/^\h*\|@key\h+(\S+)\h+(\S+)(?:\h*\v((?:(?!\|@)[\s\S])*?))(?=\h*\|@|\z)/m) do |(_, capture, ref, body)|
      components << Key.new(Term.of(Term::Sym.new(capture)), Ref.new(ref.split('.')), body)
    end

    # Find all `|@key`s without ref and turn them into `key` components.
    article.scan(/^\h*\|@key\h+(\S+)(?:\h*\v((?:(?!\|@)[\s\S])*?))(?=\h*\|@|\z)/m) do |(_, capture, body)|
      components << Key.new(Term.of(Term::Sym.new(capture)), nil, body)
    end

    # Find all `|@summary`s and turn them into `pattern` components.
    article.scan(/^\h*\|@summary\h*\v((?:(?!\v)[^\v]*(?:\v|\z))+)/m) do |(_, text)|
      components << Summary.new(text)
    end

    # Find all `|@example`s and turn them into `pattern` components.
    article.scan(/^\h*\|@example\h*\v(.+)/m) do |(_, body)|
      components << Example.new(body)
    end

    # Find all `|@block`s and turn them into `block` components. Blocks
    # must go before examples.
    article.scan(/^\h*\|@block\h*\v([\s\S]+?)((?=\|@example)|\z)/m) do |(_, body)|
      components << Block.new(body)
    end

    components
  end

  # Handles an HTTP request.
  def handle(compositions : Array(Composition), context : HTTP::Server::Context) : Nil
    page = Page.of(compositions, context)
    Page.render(context.response, page)
  end
end

module Doctool::Page
  extend self

  alias Any = Root | Overview | QueryResults | NotFound

  record Shell, title : String, body : String do
    ECR.def_to_s("src/doctool/shell.html")
  end

  record Root, roots : Hash(String, Summary) do
    ECR.def_to_s("src/doctool/root.html")

    def title : String
      "Root"
    end
  end

  record Overview, prefix : String, url : String, successors : Hash(String, Summary?), overloads : Array(Overload) do
    ECR.def_to_s("src/doctool/overview.html")

    def title : String
      prefix
    end
  end

  record QueryResults, query : String, by_ref : Set(Ref), by_example : Array({Ref, Term::Dict, Composition}) do
    ECR.def_to_s("src/doctool/query_results.html")

    def title : String
      "`#{query}`"
    end
  end

  record Overload, composition : Composition do
    ECR.def_to_s("src/doctool/overload.html")
  end

  record NotFound do
    ECR.def_to_s("src/doctool/404.html")

    def title : String
      "Not found"
    end
  end

  defrecord Successor, part : String, summary : Summary?

  def of(compositions, context : HTTP::Server::Context) : Any
    uri = context.request.uri

    case URI.decode(uri.path)
    when "/"
      root(compositions)
    when "/search"
      unless query = uri.query_params["query"]?
        return NotFound.new
      end

      query(compositions, query)
    when /^\/([\w\/<=>+*°\-]+)$/
      query = $1.split('/', remove_empty: true)
      overview(compositions, query, $1)
    else
      NotFound.new
    end
  end

  def root(compositions)
    roots0 = {} of String => Array(Summary)

    compositions.each do |composition|
      next unless ref = composition.single?(Ref)
      next unless root = ref.parts.first?

      summaries = roots0.put_if_absent(root) { [] of Summary }
      next unless ref.parts.size == 1
      next unless summary = composition.single?(Summary)

      summaries << summary
    end

    roots1 = {} of String => Summary

    roots0.each do |root, summaries|
      next unless summary = summaries.single?

      roots1[root] = summary
    end

    Root.new(roots1)
  end

  def query(compositions, query : String)
    # Find by ref.
    by_ref = Set(Ref).new
    pass do
      compositions.each do |composition|
        next unless ref = composition.single?(Ref)

        haystack = ref.parts.join('.')
        next unless haystack.includes?(query)

        by_ref << ref
      end
    end

    # Find by example.
    by_example = [] of {Ref, Term::Dict, Composition}
    pass do
      term = ML.term(query)

      found = [] of {M1::Specificity, Ref, Term::Dict, Composition}
      compositions.each do |composition|
        next unless ref = composition.single?(Ref)

        composition.each do |component|
          next unless component.is_a?(Pattern)
          next unless env = M1.match?(Term[], component.op, term)

          found << {component.specificity, ref, env, composition}
          break
        end
      end

      found.sort_by! { |specificity, _, _, _| specificity }

      found.each do |_, ref, env, composition|
        by_example << {ref, env, composition}
      end
    rescue ML::SyntaxError
    end

    QueryResults.new(query, by_ref, by_example)
  end

  ORDER = [Ref, Pattern, PatternList, Key, Summary, Block, Example]

  def overview(compositions, query, url)
    overloads = [] of Overload
    successors = {} of String => Summary?

    compositions.each do |composition|
      next unless ref = composition.single?(Ref)
      next unless ref.parts.starts_with?(query)

      unless successor = ref.parts[query.size]?
        overloads << Overload.new(composition.sort_by { |component| ORDER.index!(component.class) })
        next
      end

      next unless ref.parts.size == query.size + 1

      successors.put_if_absent(successor) do
        composition.single?(Summary)
      end
    end

    Overview.new(query.join('.'), url, successors, overloads)
  end

  def render(response : HTTP::Server::Response, page : Any) : Nil
    response.status = :ok

    render(response, Shell.new(page.title, page.to_s))
  end

  def render(response : HTTP::Server::Response, page : NotFound) : Nil
    response.status = :not_found

    render(response, Shell.new(page.title, page.to_s))
  end

  def render(response : HTTP::Server::Response, shell : Shell) : Nil
    response.print(shell)
  end
end

module Doctool
  def run : Nil
    Console.display(STDOUT, Console::WwBanner.new)
    Console.display(STDOUT, Console::InfoLog.new("Wirewright doctool"))

    compositions = Doctool::DOCS.map { |article| Doctool.composition(article) }.to_a
    compositions.sort_by! { |composition| composition.single?(Ref).try(&.parts) || [] of String }
    Console.display(STDOUT, Console::InfoLog.new("Processed #{compositions.size} composition(s)."))

    server = HTTP::Server.new do |context|
      Doctool.handle(compositions, context)
    end

    Console.display(STDOUT, Console::NoteLog.new("Serving HTTP on 127.0.0.1:9811... Hit Ctrl-C to exit."))
    server.bind("tcp://127.0.0.1:9811")
    server.listen
  end
end

Doctool.run
