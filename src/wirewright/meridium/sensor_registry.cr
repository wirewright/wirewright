module Ww::Meridium
  # A sensor registry is an emergent data structure that associates a sensor
  # conjunction apex to a sensor id. It is effectively a table with
  # the following columns:
  #
  # ```text
  #    secret     apex    sensor id     checksum
  #   primary   primary
  # ```
  #
  # Both the secret and the conjunction apex are assumed to be known by
  # the querying side implicitly. The underlying bytes multimap only stores
  # starting from the sensor id, assuming the prefix of secret followed
  # by apex implicitly. This provides an arguable degree of cryptographic
  # security to entries in the registry: only querying sides that know
  # the secret and were able to derive the apex as well are able to access
  # the sensor id.
  #
  # The sensor id acts as a connection pointer as well. In fact, only the last
  # four bytes of the sensor id identify the sensor itself; the former 12 bytes
  # identify the connection to which this sensor belongs, something akin to an
  # IP address.
  module SensorRegistry
    extend self

    Log = ::Log.for(self)

    private def register1(atoms, secret_slice, apex, sensor) : Nil
      entry = cursor = Bytes.new(secret_slice.size + Atom::BYTESIZE + WWID::BYTESIZE + sizeof(Checksum))

      cursor.copy_from(secret_slice)
      cursor += secret_slice.size

      apex.copy_hash_to(cursor)
      cursor += Atom::BYTESIZE

      sensor.to_slice_be(cursor)
      cursor += WWID::BYTESIZE

      # Compute checksum. Note how we leave the secret unhashed. This acts as a
      # tiny protection against hash collision for secrets: now both the hash
      # and the checksum computed from raw secret must collide, which is a bit
      # less likely I suppose. Although we're walking on very shaky ground
      # here anyway.
      checksum = Digest::CRC32.checksum(entry[...-sizeof(Checksum)])

      IO::ByteFormat::BigEndian.encode(checksum, cursor)
      cursor += sizeof(Checksum)

      key = entry[0, secret_slice.size + Atom::BYTESIZE]
      data = entry[secret_slice.size + Atom::BYTESIZE, WWID::BYTESIZE + sizeof(Checksum)]

      BytesMultimap.add(atoms, :sensor_registry, key, data)
    end

    # Creates a record in the sensor registry, pointing each of *apexes* to
    # the given *sensor* under *secret*.
    #
    # *mt* specifies whether to run under a multi-threaded or single-threaded
    # fiber execution context.
    def register(
      atoms : IAtomAppend,
      secret : Term?,
      apexes : Indexable(Atom),
      sensor : WWID, *,
      mt : Bool
    ) : Nil
      secret_slice = secret_to_bytes(secret)

      wg = WaitGroup.new(apexes.size)
      ctx = mt ? MT : ST

      apexes.each do |apex|
        ctx.spawn do
          register1(atoms, secret_slice, apex, sensor)
        ensure
          wg.done
        end
      end

      wg.wait
    end

    private def complete1(atoms, secret_slice, apex, mt, fn) : Nil
      key = cursor = Bytes.new(secret_slice.size + Atom::BYTESIZE)

      cursor.copy_from(secret_slice)
      cursor += secret_slice.size

      apex.copy_hash_to(cursor)
      cursor += Atom::BYTESIZE

      # Key is readonly from this point onward. It must be, since we access
      # it from complete() callback which could be called from another
      # fiber/thread.

      BytesMultimap.complete(atoms, :sensor_registry, key, prefix: Bytes.empty, mt: mt) do |completion, _|
        entry = completion.final(key, prefix: Bytes.empty)

        next if entry.size == key.size # No completions

        # Verify size
        unless entry.size == (expected = key.size + WWID::BYTESIZE + sizeof(Checksum))
          Log.debug { "reject entry: size too small (#{entry.size} != #{expected})" }
          next
        end

        # Check checksum
        checksum0 = IO::ByteFormat::BigEndian.decode(Checksum, entry[-sizeof(Checksum)..])
        checksum1 = Digest::CRC32.checksum(entry[...-sizeof(Checksum)])

        unless checksum0 == checksum1
          Log.debug { "reject entry: checksum mismatch (my #{checksum1} != its #{checksum0})" }
          next
        end

        # Decode sensor id
        sensor_slice = entry[key.size, WWID::BYTESIZE]

        begin
          sensor = WWID.from_slice_be(sensor_slice)
        rescue e : WWID::ParseError
          Log.debug(exception: e) { "reject entry" }
          next
        end

        # Call fn with sensor id
        fn.call(sensor)
      end
    end

    # Calls *fn* with each sensor registered at each of *apexes* under *secret*.
    # Sensors may repeat if one sensor is registered at multiple *apexes*.
    #
    # *mt* specifies whether to run under a multi-threaded or single-threaded
    # fiber execution context.
    #
    # WARNING: *fn* will be called from another fiber, perhaps running on another
    # thread if *mt* is `true` (it is by default). Thus make sure to either have
    # fully compartmentalized *fn*, or *fn* that talks to the outside world in a
    # thread-safe manner.
    def each_sensor(
      atoms : IAtomsPresent,
      secret : Term?,
      apexes : Indexable(Atom), *,
      mt : Bool,
      &fn : WWID ->
    ) : Nil
      secret_slice = secret_to_bytes(secret)

      ctx = mt ? MT : ST
      wg = WaitGroup.new(apexes.size)

      apexes.each do |apex|
        ctx.spawn do
          complete1(atoms, secret_slice, apex, mt, fn)
        ensure
          wg.done
        end
      end

      wg.wait
    end
  end
end
