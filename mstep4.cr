require "./src/wirewright"

class Meridium::StepSpace
  # :nodoc:
  STATE0 = Term[sensors: Term[], appearances: Term[]]

  def initialize(tspace : Tspace::IFrontend, @alert : ->)
    @percepts = Term[]
    @percepts_lock = Mutex.new

    @state = STATE0

    @surfaces = SetConn.new(WWID.new, tspace) do |view|
      @percepts_lock.synchronize do
        @percepts = Term::Dict.build do |commit|
          view.each do |_, percepts|
            spec = Term[query: percepts.surface.pattern, secret: percepts.surface.secret?]

            commit.with(spec, percepts.dict_multiset)
          end
        end
      end

      @alert.call
    end
  end

  delegate :connect, :disconnect, to: @surfaces

  def sync(goal, & : Term, Term ->) : Term::Dict
    state0 = @state
    state1 = sanitize(goal)

    @percepts_lock.synchronize do
      @percepts.each_entry do |spec, mset|
        yield spec, mset
      end

      @percepts = Term[]
    end

    unless state0 == state1
      sync!(state0, state1)
    end

    @state = peek(state1)
  end

  private def sanitize(goal : Term) : Term::Dict
    unless goal = goal.as_d?
      return Term[]
    end

    sensors = goal[:sensors]?.try(&.as_d?) || Term[]
    appearances = goal[:appearances]?.try(&.as_d?) || Term[]

    Term[sensors: sanitize(Sensor, sensors), appearances: sanitize(Appearance, appearances)]
  end

  private def sanitize(cls : Sensor.class, surfaces0 : Term::Dict) : Term::Dict
    surfaces1 = surfaces0

    surfaces0.each_entry do |subject, _|
      Term.case(subject) do
        matchpi %{(¦ (%keypool period secret) query: _)} { }
        otherwise do
          surfaces1 = surfaces1.without(subject)
        end
      end
    end

    surfaces1
  end

  private def sanitize(cls : Appearance.class, surfaces0 : Term::Dict) : Term::Dict
    surfaces1 = surfaces0

    surfaces0.each_entry do |subject, _|
      Term.case(subject) do
        matchpi %{(¦ (%keypool period secret) value: _)} { }
        otherwise do
          surfaces1 = surfaces1.without(subject)
        end
      end
    end

    surfaces1
  end

  private def sync!(state0, state1)
    @surfaces.transaction do |txn|
      sync!(Sensor, state0[:sensors], state1[:sensors], txn)
      sync!(Appearance, state0[:appearances], state1[:appearances], txn)
    end

    @state = state1
  end

  private def sync!(cls, surfaces0, surfaces1, txn)
    added, removed = surfaces1.diff(surfaces0)

    removed.each_entry do |surface, _|
      txn.delete(identity(cls, surface.as_d))
    end

    added.each_entry do |surface, _|
      txn.add(identity(cls, surface.as_d))
    end
  end

  private def peek(state : Term::Dict) : Term::Dict
    Term[sensors: peek(Sensor, state[:sensors].as_d), appearances: peek(Appearance, state[:appearances].as_d)]
  end

  private def peek(cls, surfaces0 : Term::Dict) : Term::Dict
    surfaces1 = surfaces0
    surfaces0.each_entry do |surface, _|
      surfaces1 = surfaces1.with(surface, @surfaces.sync?(identity(cls, surface.as_d)))
    end
    surfaces1
  end

  # TODO: handle period
  private def identity(cls : Sensor.class, surface : Term::Dict)
    Sensor.new(surface[:query], secret: surface[:secret]?, relook: 1.minute)
  end

  private def identity(cls : Appearance.class, surface : Term::Dict)
    Appearance.new(surface[:value], secret: surface[:secret]?)
  end
end

class Meridium::Step
  Log = ::Log.for(self)

  def initialize
    @tspaces = {} of Term => StepSpace
    @lock = Mutex.new
  end

  def register(name : Term, tspace tspace1 : StepSpace) : Nil
    @lock.synchronize do
      if tspace0 = @tspaces[name]?
        tspace0.disconnect
      end

      @tspaces[name] = tspace1

      tspace1.connect
    end
  end

  def unregister(name : Term) : Nil
    @lock.synchronize do
      return unless tspace = @tspaces.delete(name)

      tspace.disconnect
    end
  end

  private def rendezvous(document : Term::Dict) : Term::Dict
    events = Rhodium::Q.of(document, Rhodium::Events)

    tspaces0 = document[Rhodium::Tspaces]? || Term[]
    tspaces1 = tspaces0

    tspaces0.each_entry do |name, goal|
      next unless tspace0 = @tspaces[name]?

      tspace1 = tspace0.sync(goal) do |spec, mset|
        event = Term.of(:percepts, name, spec, mset)
        events = events.enqueue(event)
      end

      tspaces1 = tspaces1.with(name, tspace1)
    end

    document = document.with(Rhodium::Tspaces, tspaces1)

    events.commit(document, Rhodium::Events)
  end

  def fn : D7::Step
    D7::Step.new do |document|
      {rendezvous(document), false}
    end
  end
end

# include Meridium

# tspace = Tspace::InMemory.new
# space = Meridium::StepSpace.new(tspace, -> { pp "WAKE UP" })
# space.connect

# goal = ML.dict <<-WWML
# sensors: { (query: x_number): true, },
# appearances: { (value: 100): true, }
# WWML

# space.sync(goal)

# goal = ML.dict <<-WWML
# sensors: { (query: x_number): true, },
# appearances: { (value: 101): true, }
# WWML

# space.sync(goal)

# pp space
