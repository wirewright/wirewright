module Ww::Rack::Database
  extend self

  # :nodoc:
  defcase State,
    epoch : Automaton::Epoch,
    connections : Hash(URI, Connection),
    tasks : D7::TaskBoard(Automaton::Epoch, Task, Term)

  alias Connection = Up | Dn | Pending

  defrecord Up, db : DB::Database
  defrecord Dn, detail : String
  defrecord Pending

  defrecord Task, uri : URI, db : DB::Database, stmt : Stmt

  def state(epoch : Automaton::Epoch) : State
    connections = {} of URI => Connection

    tasks = D7::TaskBoard(Automaton::Epoch, Task, Term).new(epoch) do |task, ping|
      execute(task.db, task.stmt, ping)
    end

    State.new(epoch, connections, tasks)
  end

  # NOTE: `state.connections` never has pending connections in this implementation
  # because we handle them before returning from `step(State, &)`.
  def pending?(state : State) : Bool
    state.tasks.pending?
  end

  defrecord StepContext,
    uris : Set(URI),
    tasks : D7::TaskBoard::Rdv(Automaton::Epoch, Task, Term)

  def step(state : State, & : Propose -> T) : T forall T
    seen_uris = Set(URI).new

    result = state.tasks.rdv do |tasks_rdv|
      ctx = StepContext.new(seen_uris, tasks_rdv)
      propose = Propose.new do |hg, proposals|
        propose(state, ctx, hg, proposals)
      end
      yield propose
    end

    state.connections.diff(seen_uris) do |action|
      uri = action.key

      case action
      in Hash::DiffAdded
        begin
          db = DB.open(uri)
          connection = Up.new(db)
        rescue e : DB::Error | ArgumentError
          connection = Dn.new(e.message || "internal database error")
        end

        state.connections[uri] = connection
        state.epoch.call
      in Hash::DiffRemoved
        connection = action.value

        case connection
        in Dn, Pending
        in Up
          connection.db.close
        end

        state.connections.delete(uri)
        state.epoch.call
      end
    end

    result
  end

  alias Variant = Transfer

  defrecord Transfer,
    node : D7::Node,
    stmt : D7::AbsEdge,
    uri : URI,
    response : D7::AbsEdge

  private def propose(state : State, ctx : StepContext, hg : D7::Hypergraph, proposals) : Nil
    hg.propose(proposals, :db) do |node|
      Term.case(node.term) do
        matchpi %{[db (@stmt_ -> uri_string -> @response_) _?]}, uri: String do
          variant = Transfer.new(node, hg.resolve(node.addr, stmt), URI.parse(uri), hg.resolve(node.addr, response))
          step(state, ctx, hg, variant)
        end

        otherwise { }
      end
    end
  end

  defrecord Source, node : D7::Node, stmt : Stmt

  private def source?(hg : D7::Hypergraph, input : D7::AbsEdge) : Source?
    return unless cell = Rack.cell?(hg, input)
    return unless stmtQ = cell.value?
    return unless stmt = stmt?(stmtQ)

    Source.new(cell.node, stmt)
  end

  private def step(state : State, ctx : StepContext, hg : D7::Hypergraph, variant : Transfer) : D7::Patch?
    ctx.uris << variant.uri

    case connection = state.connections[variant.uri]?
    in Nil, Pending
      return D7.patch(variant.node, {2, :pending})
    in Dn
      return D7.patch(variant.node, {2, {:dn, connection.detail}})
    in Up
      status_patch = D7.patch(variant.node, {2, :up})
    end

    unless source = source?(hg, variant.stmt)
      return status_patch
    end

    # Find empty target cell(s).
    targets = Pf::Kit.stack_array(D7::Node, 1)

    Rack.each_cell(hg, variant.response) do |cell|
      next unless cell.empty?

      targets << cell.node
    end

    if targets.empty?
      return status_patch
    end

    task = Task.new(variant.uri, connection.db, source.stmt)

    if result = ctx.tasks.result?(task)
      return D7.patches(status_patch,
        D7.patch(source.node, {2, nil}),
        D7.patches(targets, {2, result}),
      )
    end

    ctx.tasks.publish(task)

    status_patch
  end

  alias Stmt = Exec | Query

  defrecord Exec, sql : String, args : Slice(DB::Any)
  defrecord Query, sql : String, args : Slice(DB::Any)

  # Attempts to recognize a database statement in *term*.
  private def stmt?(term : Term) : Stmt?
    Term.case(term) do
      # |@ rack.db.stmt
      #
      # |@pattern
      # (exec sql_string args_*)
      #
      # |@key sql
      # The SQL string to execute.
      #
      # |@key args
      # Values for each "blank" in the SQL string.
      #
      # |@block
      # Executes an SQL statement.
      #
      # |@example
      # ```wwml
      # ;; SQLite
      # (exec "insert into people values (?, ?)" "Jane Doe" 36)
      # ```
      matchpi %{(exec sql_string rest_*)}, sql: String do
        Exec.new(sql, transcribe(rest.items))
      end

      # |@ rack.db.stmt
      #
      # |@pattern
      # (query sql_string args_*)
      #
      # |@key sql
      # The SQL string to execute.
      #
      # |@key args
      # Values for each "blank" in the SQL string.
      #
      # |@block
      # Executes an SQL query.
      #
      # |@example
      # ```wwml
      # ;; SQLite
      # (query "select name, age from people whee age > ?" 36)
      # ```
      matchpi %{(query sql_string rest_*)}, sql: String do
        Query.new(sql, transcribe(rest.items))
      end

      otherwise { }
    end
  end

  # Runs *stmt* against *db*. Blocks until completion. *ping* is called whenever
  # possible to allow cancellation.
  private def execute(db : DB::Database, stmt : Exec, ping) : Term
    result = db.exec(stmt.sql, args: stmt.args)

    Term.of(:ok, result.rows_affected)
  rescue e : SQLite3::Exception
    Term.of(:err, e.message || "internal sqlite3 error")
  end

  # :ditto:
  private def execute(db : DB::Database, stmt : Query, ping) : Term
    rows = Term::Dict.build do |commit|
      commit << :ok

      db.query(stmt.sql, args: stmt.args) do |rs|
        rs.each do
          ping.call
          commit << transcribe(rs)
        end
      end
    end

    Term.of(rows)
  rescue e : SQLite3::Exception
    Term.of(:err, e.message || "internal sqlite3 error")
  end

  private def transcribe(terms : Enumerable(Term)) : Slice(DB::Any)
    terms.to_readonly_slice { |term| transcribe(term) }
  end

  # Converts `Term` to `DB::Any`.
  private def transcribe(term : Term) : DB::Any
    result = Term.case(term) do
      matchpi %{_string} { term.to(String) }
      matchpi %{_boolean} { term.to(Bool) }
      matchpi %{(%number i32)} { term.to(Int32) }
      matchpi %{(%number i64)} { term.to(Int64) }

      matchpi %{_number} do
        n = term.as_n
        continue unless n.approx?

        # There's little point going through Float64 as well because we store
        # approx as Float32 so there's no more "precision" or anything to gain.
        n.to(Float32)
      end

      matchpi %{_blob} do
        term.as_blob.to_slice
      end

      otherwise do
        ML.compact(term) # Fallback
      end
    end

    result.as(DB::Any)
  end

  # WARNING: mutates (advances through) *rs*.
  private def transcribe(object rs : DB::ResultSet) : Term
    result = Term::Dict.build do |commit|
      (0...rs.column_count).each do |index|
        key = Term.of(rs.column_name(index))
        value = transcribe?(rs.read)
        commit.with(key, value)
      end
    end

    Term.of(result)
  end

  private def transcribe?(object : DB::Any) : Term?
    # Term.of handles all members of DB::Any.
    Term.of(object)
  end
end
