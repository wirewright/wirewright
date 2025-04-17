class Meridium::Step
  Log = ::Log.for(self)

  def initialize
    @spaces = {} of Term => StepSpace
  end

  def register(name : Term, space space1 : StepSpace) : Nil
    if space0 = @spaces[name]?
      space0.teardown
    end

    @spaces[name] = space1
  end

  def unregister(name : Term) : Nil
    return unless space = @spaces.delete(name)

    space.teardown
  end

  private def dispatch(document : Term::Dict)
    return unless tspaces = document[Rhodium::Tspaces]?

    Log.trace { "dispatch on document with #{tspaces.size} termspace(s)"}

    @spaces.each do |name, space|
      content = tspaces[name]? || Term.of

      space.sync(content)
    end
  end

  def fn : D7::Step
    D7::Step.new do |document|
      dispatch(document)

      {document, false}
    end
  end
end

class Meridium::StepSpace
  Log = ::Log.for(self)

  def initialize(@document : Document, @setconn : Tsetconn, @name : Term)
    @sensors0 = @appearances0 = Term[]

    @msets = {} of Slot => Term::Dict
    @msets_lock = Mutex.new
  end

  def self.inmemory(document : Document, name : Term)
    space = nil
    observer = ->(view : Tview) do
      space.not_nil!("BUG: space not ready to receive views").observe(view)
    end

    set = TspaceDigestSet.new(SyncInMemoryMultiset(Bytes).new)
    chat = SyncInMemoryChat(Activation).new
    conn = Tconn.new(set, chat, observer)
    setconn = Tsetconn.new(conn)
    space = instance = new(document, setconn, name)

    instance
  end

  def self.tcp(document : Document,
               host : String,
               port : Int32,
               name : Term, *,
               keepalive = Keepalive::Continuous.new(30.seconds),
               relook = Relook::Periodic.new)
    space = nil
    observer = ->(view : Tview) do
      space.not_nil!("BUG: space not ready to receive views").observe(view)
    end

    server = RemoteSurfnetServer.new(host, port)

    set = TspaceDigestSet.new(UnbufferedSet.new(server))
    chat = server
    conn = Tconn.new(set, chat, observer, keepalive: keepalive, relook: relook)
    setconn = Tsetconn.new(conn)
    space = instance = new(document, setconn, name)

    instance
  end

  def teardown : Nil
    @setconn.close
  end

  # WARNING: This method could run in another fiber, but could also run during
  # `sync`. Or both.
  def observe(view : Tview) : Nil
    Log.debug { "received view #{view}" }

    view.each do |slot, view|
      next unless sensor = @setconn.sensor?(slot)

      mset1 = view.dict_multiset

      # At this point we must guarantee that equal multisets won't be emitted.
      updated = @msets_lock.synchronize do
        if @msets[slot]? != mset1
          @msets[slot] = mset1

          true
        else
          false
        end
      end

      next unless updated

      event = Term.of(:stimuli, @name, {sensor.pattern, sensor.secret}, mset1)
      prompt = Term.of(:event, event)

      @document.send(prompt)
    end
  end

  def sync(content : Term)
    Log.trace { "receive sync() request with #{ML.compact(content)} "}

    if content = content.as_d?
      sensors = content[:sensors]?
      appearances = content[:appearances]?
    end

    sync_sensors(sensors || Term.of)
    sync_appearances(appearances || Term.of)
  end

  private def sync_sensors(sensors1 : Term) : Nil
    unless sensors1 = sensors1.as_d?
      Log.warn { "sensors were replaced by non-dict term, treating as empty dict" }
      sensors1 = Term[]
    end

    inserted, removed = sensors1.diff(@sensors0)

    removed.each_entry { |surface, _| delete_sensor(surface) }
    inserted.each_entry { |surface, _| insert_sensor(surface) }

    @sensors0 = sensors1
  end

  private def sync_appearances(appearances1 : Term) : Nil
    unless appearances1 = appearances1.as_d?
      Log.warn { "appearances were replaced by non-dict term, treating as empty dict" }
      appearances1 = Term[]
    end

    inserted, removed = appearances1.diff(@appearances0)

    removed.each_entry { |surface, _| delete_appearance(surface) }
    inserted.each_entry { |surface, _| insert_appearance(surface) }

    @appearances0 = appearances1
  end

  private def insert_sensor(surface : Term)
    unless (spec = surface.as_d?) && spec.size.in?(1, 2)
      Log.warn { "ignoring invalid sensor spec: #{ML.compact(surface)}"}
      return
    end

    pattern = spec[0]
    secret = spec[1]?

    @setconn << Tsetconn::Sensor.new(pattern, secret)

    Log.debug { "inserted sensor #{ML.compact(spec)}" }
  end

  private def delete_sensor(surface : Term)
    unless (spec = surface.as_d?) && spec.size.in?(1, 2)
      Log.warn { "ignoring invalid sensor spec: #{ML.compact(surface)}"}
      return
    end

    pattern = spec[0]
    secret = spec[1]?

    @setconn.delete(Tsetconn::Sensor.new(pattern, secret))

    Log.debug { "removed sensor #{ML.compact(spec)}" }
  end

  private def insert_appearance(surface : Term)
    unless (spec = surface.as_d?) && spec.size.in?(1, 2)
      Log.warn { "ignoring invalid sensor spec: #{ML.compact(surface)}"}
      return
    end

    value = spec[0]
    secret = spec[1]?

    @setconn << Tsetconn::Appearance.new(value, secret)

    Log.debug { "inserted appearance #{ML.compact(spec)}" }
  end

  private def delete_appearance(surface : Term)
    unless (spec = surface.as_d?) && spec.size.in?(1, 2)
      Log.warn { "ignoring invalid sensor spec: #{ML.compact(surface)}"}
      return
    end

    value = spec[0]
    secret = spec[1]?

    @setconn.delete(Tsetconn::Appearance.new(value, secret))

    Log.debug { "removed appearance #{ML.compact(spec)}" }
  end
end
