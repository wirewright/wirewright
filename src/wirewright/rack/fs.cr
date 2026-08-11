require "file_utils"

# Implements file system manipulation support for Rack. See also `rack.fs`.
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
    # |@ rack.fs.request
    #
    # |@summary
    # Requests that you can make to the file system.
    Term.case(term) do
      # |@ rack.fs.request
      #
      # |@pattern
      # [create file path_string]
      #
      # |@block
      # Creates a file at *path* if missing. This works like the shell
      # command `touch`.
      matchpi %{[create file path_string]}, path: NormalPath do
        CreateFile.new(path)
      end

      # |@ rack.fs.request
      #
      # |@pattern
      # [create dir path_string]
      #
      # |@block
      # Creates a directory at *path*. Fails if *path* exists. This works
      # like the shell command `mkdir`.
      matchpi %{[create dir path_string]}, path: NormalPath do
        CreateDir.new(path)
      end

      # |@ rack.fs.request
      #
      # |@pattern
      # [create dir if missing path_string]
      #
      # |@block
      # Creates a directory at *path* if it is missing.
      matchpi %{[create dir if missing path_string]}, path: NormalPath do
        CreateDirIfMissing.new(path)
      end

      # |@ rack.fs.request
      #
      # |@pattern
      # [delete file path_string]
      #
      # |@block
      # Removes the file at *path*. Fails if *path* does not exist or is
      # a directory.
      matchpi %{[delete file path_string]}, path: NormalPath do
        DeleteFile.new(path)
      end

      # |@ rack.fs.request
      #
      # |@pattern
      # [delete file if exists path_string]
      #
      # |@block
      # Removes the file at *path* if present. Fails if *path* is a directory.
      matchpi %{[delete file if exists path_string]}, path: NormalPath do
        DeleteFileIfExists.new(path)
      end

      # |@ rack.fs.request
      #
      # |@pattern
      # (delete dir path_string ⍊ recursive⋮ false)
      #
      # |@key recursive
      # Whether to delete recursively (e.g. `rm` vs. `rm -r`). If `false`, fails
      # if *path* is a nonempty directory.
      #
      # |@block
      # Removes the directory at *path*. Fails if *path* does not exist or is a file.
      matchpi %{(delete dir path_string ⍊ recursive⋮ false)}, path: NormalPath do
        DeleteDir.new(path, recursive.true?)
      end

      # |@ rack.fs.request
      #
      # |@pattern
      # (delete dir if exists path_string ⍊ recursive⋮ false)
      #
      # |@key recursive
      # Whether to delete recursively (e.g. `rm` vs. `rm -r`). If `false`, fails
      # if *path* is a nonempty directory.
      #
      # |@block
      # Removes the directory at *path* if present. Fails if *path* is a file.
      matchpi %{(delete dir if exists path_string ⍊ recursive⋮ false)}, path: NormalPath do
        DeleteDirIfExists.new(path, recursive.true?)
      end

      # |@ rack.fs.request
      #
      # |@pattern
      # [move src_string to dst_string]
      #
      # |@block
      # Moves a file system entry from a source path *src* to a destination path
      # *dst*. If *dst* is on another device, *src* is copied to *dst* and removed.
      matchpi %{[move src_string to dst_string]}, src: NormalPath, dst: NormalPath do
        Move.new(src, dst)
      end

      # |@ rack.fs.request
      #
      # |@pattern
      # [create temporary file in parent_string]
      #
      # |@key parent
      # Path to the directory where the temporary file should be created.
      #
      # |@block
      # Creates a temporary file in the given *parent* directory. Its name can
      # be retrieved from the `rack.fs.response` (`present`). You are responsible
      # for eventually removing it.
      matchpi %{[create temporary file in parent_string]}, parent: NormalPath do
        CreateTmpFile.new(parent)
      end

      # |@ rack.fs.request
      #
      # |@pattern
      # [overwrite file path_string content_blob]
      # [overwrite file path_string content_string]
      #
      # |@block
      # Overwrites the contents of the file at *path* with *content*. The file
      # will be created if missing.
      matchpi %{[overwrite file path_string content_]}, path: NormalPath do |content|
        continue unless content = content.as_s? || content.as_blob?

        OverwriteFile.new(path, content)
      end

      # |@ rack.fs.request
      #
      # |@pattern
      # [append to file path_string content_blob]
      # [append to file path_string content_string]
      #
      # |@block
      # Appends *content* to the file at *path*. The file will be created
      # if missing.
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
    # |@ rack.fs.response
    #
    # |@summary
    # The possible responses to a request.
    case response
    in Present
      # |@ rack.fs.response
      #
      # |@pattern
      # (ok (present path_string))
      #
      # |@block
      # Confirms that an action was carried out after which *path* was
      # observed present.
      Term.of(:ok, {:present, response.path})
    in Absent
      # |@ rack.fs.response
      #
      # |@pattern
      # (ok (absent path_string))
      #
      # |@block
      # Confirms that an action was carried out after which *path* was
      # observed absent.
      Term.of(:ok, {:absent, response.path})
    in Wrote
      # |@ rack.fs.response
      #
      # |@pattern
      # (ok (wrote path_string))
      #
      # |@block
      # Confirms that an action was carried out after which the content
      # of the file at *path* changed.
      Term.of(:ok, {:wrote, response.path})
    in Moved
      # |@ rack.fs.response
      #
      # |@pattern
      # (ok (moved src_string dst_string))
      #
      # |@block
      # Confirms that the file system entry at *src* was moved to *dst*.
      Term.of(:ok, {:moved, response.src, response.dst})
    in Err
      # |@ rack.fs.response
      #
      # |@pattern
      # (err detail_string)
      #
      # |@block
      # Signals that an error occurred while performing a request. *detail*
      # explains the reason.
      Term.of(:err, response.detail)
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

  def step(state : State, & : Propose -> T) : T forall T
    result = state.tasks.rdv do |tasks_rdv|
      propose = Propose.new do |hg, proposals|
        propose(tasks_rdv, hg, proposals)
      end
      yield propose
    end
  end

  private def propose(tasks, hg : D7::Hypergraph, proposals) : Nil
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
