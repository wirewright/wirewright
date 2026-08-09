require "file_utils"

# Implements file system manipulation support for Rack. Essentially, a file
# system manipulation machine. See also `rack.fs`.
#
# Actual, real-world file systems are really hard to represent declaratively on
# the write end (and on the read end, too, but less so, so for that we have `rack.path`).
# Instead, we provide an imperative way to manipulate the file system. In effect,
# `rack.fs` is a client-server approach to file systems.
module ::Ww::Rack::FS
  extend self

  defcase State, tasks : D7::TaskBoard(Automaton::Epoch, Request, Response)

  alias Request = CreateFile | CreateDir | CreateDirIfMissing | DeleteFile | DeleteFileIfExists |
                  DeleteDir | DeleteDirIfExists | Move | CreateTmpFile | OverwriteFile | AppendFile

  defrecord CreateFile, path : NormalPath
  defrecord CreateDir, path : NormalPath
  defrecord CreateDirIfMissing, path : NormalPath
  defrecord DeleteFile, path : NormalPath
  defrecord DeleteFileIfExists, path : NormalPath
  defrecord DeleteDir, path : NormalPath, recursive : Bool
  defrecord DeleteDirIfExists, path : NormalPath, recursive : Bool
  defrecord Move, src : NormalPath, dst : NormalPath
  defrecord CreateTmpFile, parent : NormalPath
  defrecord OverwriteFile, path : NormalPath, content : Term::Str | Term::Blob
  defrecord AppendFile, path : NormalPath, content : Term::Str | Term::Blob

  def request?(term : Term) : Request?
    Term.case(term) do
      matchpi %{[create file path_string]}, path: NormalPath do
        CreateFile.new(path)
      end

      matchpi %{[create dir path_string]}, path: NormalPath do
        CreateDir.new(path)
      end

      matchpi %{[create dir if missing path_string]}, path: NormalPath do
        CreateDirIfMissing.new(path)
      end

      matchpi %{[delete file path_string]}, path: NormalPath do
        DeleteFile.new(path)
      end

      matchpi %{[delete file if exists path_string]}, path: NormalPath do
        DeleteFileIfExists.new(path)
      end

      matchpi %{(delete dir path_string ⍊ recursive⋮ false)}, path: NormalPath do
        DeleteDir.new(path, recursive.true?)
      end

      matchpi %{(delete dir if exists path_string ⍊ recursive⋮ false)}, path: NormalPath do
        DeleteDirIfExists.new(path, recursive.true?)
      end

      matchpi %{[move src_string to dst_string]}, src: NormalPath, dst: NormalPath do
        Move.new(src, dst)
      end

      matchpi %{[create temporary file in parent_string]}, parent: NormalPath do
        CreateTmpFile.new(parent)
      end

      matchpi %{[overwrite file path_string content_]}, path: NormalPath do |content|
        continue unless content = content.as_s? || content.as_blob?

        OverwriteFile.new(path, content)
      end

      matchpi %{[append to file path_string content_]}, path: NormalPath do |content|
        continue unless content = content.as_s? || content.as_blob?

        AppendFile.new(path, content)
      end

      otherwise { }
    end
  end

  alias Response = Present | Absent | Moved | Wrote | Err

  defrecord Present, path : NormalPath
  defrecord Absent, path : NormalPath
  defrecord Moved, src : NormalPath, dst : NormalPath
  defrecord Wrote, path : NormalPath
  defrecord Err, detail : String

  def render(response : Response) : Term
    case response
    in Present then Term.of(:ok, {:present, response.path})
    in Absent  then Term.of(:ok, {:absent, response.path})
    in Wrote   then Term.of(:ok, {:wrote, response.path})
    in Moved   then Term.of(:ok, {:moved, response.src, response.dst})
    in Err     then Term.of(:err, response.detail)
    end
  end

  def state(epoch : Automaton::Epoch) : State
    tasks = D7::TaskBoard(Automaton::Epoch, Request, Response).new(epoch) do |task, ping|
      execute(task, ping)
    end

    State.new(tasks)
  end

  def pending?(state : State) : Bool
    state.tasks.pending?
  end

  def step(state : State, & : Proposer -> T) : T forall T
    result = state.tasks.rdv do |tasks_rdv|
      yield Proposer.new(tasks_rdv)
    end
  end

  struct Proposer
    def initialize(@tasks : D7::TaskBoard::Rdv(Automaton::Epoch, Request, Response))
    end

    def propose(hg : D7::Hypergraph, proposals) : Nil
      FS.propose(@tasks, hg, proposals)
    end
  end

  # :nodoc:
  def propose(tasks, hg : D7::Hypergraph, proposals) : Nil
    hg.propose(proposals, :fs) do |node|
      Term.matchpi?(node.term, %{[fs @request_ @response_]}) do
        abs_request = hg.resolve(node.addr, request)
        abs_response = hg.resolve(node.addr, response)
        machine = Machine.new(abs_request, abs_response)
        step(tasks, hg, node, machine)
      end
    end
  end

  defrecord Machine, request : D7::AbsEdge, response : D7::AbsEdge

  private def step(tasks, hg, node : D7::Node, machine : Machine) : D7::Patch?
    return unless request_cell = Rack.cell?(hg, machine.request)
    return unless requestQ = request_cell.value?

    return unless response_cell = Rack.cell?(hg, machine.response)
    return unless response_cell.value?.nil? # response cell must be empty

    return unless request = request?(requestQ)

    if response = tasks.result?(request)
      return D7.patches(
        D7.patch(request_cell.node, {2, nil}),
        D7.patch(response_cell.node, {2, render(response)}),
      )
    end

    tasks.publish(request)
  end

  private def execute(task : CreateFile, ping : D7::TaskBoard::Ping) : Response
    FileUtils.touch(task.path.unwrap)
    Present.new(task.path)
  rescue e : File::Error | IO::Error
    Err.new(e.message || "internal error")
  end

  private def execute(task : CreateDir, ping : D7::TaskBoard::Ping) : Response
    FileUtils.mkdir(task.path.unwrap)
    Present.new(task.path)
  rescue e : File::Error | IO::Error
    Err.new(e.message || "internal error")
  end

  private def execute(task : CreateDirIfMissing, ping : D7::TaskBoard::Ping) : Response
    FileUtils.mkdir(task.path.unwrap)
    Present.new(task.path)
  rescue e : File::Error | IO::Error
    Err.new(e.message || "internal error")
  end

  private def execute(task : DeleteFile, ping : D7::TaskBoard::Ping) : Response
    FileUtils.rm(task.path.unwrap)
    Absent.new(task.path)
  rescue e : File::Error | IO::Error
    Err.new(e.message || "internal error")
  end

  private def execute(task : DeleteFileIfExists, ping : D7::TaskBoard::Ping) : Response
    FileUtils.rm_f(task.path.unwrap)
    Absent.new(task.path)
  rescue e : File::Error | IO::Error
    Err.new(e.message || "internal error")
  end

  private def execute(task : DeleteDir, ping : D7::TaskBoard::Ping) : Response
    if task.recursive
      FileUtils.rm_r(task.path.unwrap)
    else
      FileUtils.rmdir(task.path.unwrap)
    end

    Absent.new(task.path)
  rescue e : File::Error | IO::Error
    Err.new(e.message || "internal error")
  end

  private def execute(task : DeleteDirIfExists, ping : D7::TaskBoard::Ping) : Response
    begin
      if task.recursive
        FileUtils.rm_r(task.path.unwrap)
      else
        FileUtils.rmdir(task.path.unwrap)
      end
    rescue File::NotFoundError
    end

    Absent.new(task.path)
  rescue e : File::Error | IO::Error
    Err.new(e.message || "internal error")
  end

  private def execute(task : Move, ping : D7::TaskBoard::Ping) : Response
    FileUtils.mv(task.src.unwrap, task.dst.unwrap)
    Moved.new(task.src, task.dst)
  rescue e : File::Error | IO::Error
    Err.new(e.message || "internal error")
  end

  private def execute(task : CreateTmpFile, ping : D7::TaskBoard::Ping) : Response
    name = File.tempname(prefix: "ww", suffix: nil, dir: task.parent.unwrap.to_s)
    File.touch(name)
    Present.new(NormalPath[task.parent / name])
  rescue e : File::Error | IO::Error
    Err.new(e.message || "internal error")
  end

  private def execute(task : OverwriteFile, ping : D7::TaskBoard::Ping) : Response
    case content = task.content
    in Term::Str # Assume UTF-8 / text
      File.write(task.path.unwrap, content.to(String), mode: "w")
    in Term::Blob # Assume binary data
      File.open(task.path.unwrap, mode: "wb", &.write(content.to_slice))
    end

    Wrote.new(task.path)
  rescue e : File::Error | IO::Error
    Err.new(e.message || "internal error")
  end

  private def execute(task : AppendFile, ping : D7::TaskBoard::Ping) : Response
    case content = task.content
    in Term::Str # Assume UTF-8 / text
      File.write(task.path.unwrap, content.to(String), mode: "a")
    in Term::Blob # Assume binary data
      File.open(task.path.unwrap, mode: "ab", &.write(content.to_slice))
    end

    Wrote.new(task.path)
  rescue e : File::Error | IO::Error
    Err.new(e.message || "internal error")
  end
end
