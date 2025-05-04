module Ww::Meridium
  # AppearanceRegistry manages associations between strands and appearance ids
  # under a secret, allowing registration of appearance ids for values and
  # querying for appearance ids common to multiple strands.
  module AppearanceRegistry
    extend self

    Log = ::Log.for(self)

    # :nodoc:
    APPEARANCE_SET_GAP = "appearances".to_slice

    private def mount1(atoms, secret_slice, ubases, appearance) : Nil
      scratch = uninitialized UInt8[WWID::BYTESIZE]

      data = Ubase.upack(ubases)

      # Compute checksum (incorporating both secret and data bytes)
      checksum = Digest::CRC32.initial
      checksum = Digest::CRC32.update(secret_slice, checksum)
      checksum = Digest::CRC32.update(data, checksum)

      # Append data bytes.
      terminal = BytesMM.add(atoms, :appearance_registry, secret_slice, data: data)

      # Append checksum bytes.
      IO::ByteFormat::BigEndian.encode(checksum, scratch.to_slice)
      terminal = BytesMM.add(atoms, terminal, scratch.to_slice[0, sizeof(UInt32)])

      # Append gap indicator. The other side will have to bridge the gap on its own.
      terminal = BytesMM.append(terminal, APPEARANCE_SET_GAP)

      # Append appearance id.
      appearance.to_slice_be(scratch.to_slice)
      BytesMM.add(atoms, terminal, scratch.to_slice)
    end

    # Subscribes *appearance* to sensors perceiving *value* under *secret*.
    def mount(atoms : IAtomAppend, secret : Term?, value : Term, appearance : WWID) : Nil
      secret_slice = Meridium.secret_slice(secret)

      wg = WaitGroup.new

      Term.each_keypath_and_leaf(value) do |keypath, leaf|
        ubases = Ubase.strand(keypath, leaf)

        wg.spawn { mount1(atoms, secret_slice, ubases, appearance) }

        true # continue
      end

      wg.wait
    end

    private def bundleof(atoms, secret_slice, strand) : Array(BytesMM::Row)
      prefix = Ubase.upack(strand)

      # The completion callback runs on different threads (that is, it may).
      # So we must synchronize somehow.
      bundle = [] of BytesMM::Row
      lock = Mutex.new

      BytesMM.complete(atoms, :appearance_registry, secret_slice, prefix) do |completion, atom|
        # We must at least complete the checksum.
        next if completion.bytesize < sizeof(UInt32)

        suffix = completion.final(key: Bytes.empty, prefix: Bytes.empty)

        checksum0 = IO::ByteFormat::BigEndian.decode(UInt32, suffix[-sizeof(UInt32)..])

        checksum1 = Digest::CRC32.initial
        checksum1 = Digest::CRC32.update(secret_slice, checksum1)
        checksum1 = Digest::CRC32.update(prefix, checksum1)
        checksum1 = Digest::CRC32.update(suffix[...-sizeof(UInt32)], checksum1)

        unless checksum0 == checksum1
          Log.debug { "reject entry: checksum mismatch (my #{checksum1} != its #{checksum0})" }
          next
        end

        # Remember that we insert an artificial gap between the strand bytes +
        # checksum and the set of appearances subscribed to that strand (represented
        # as a digit trie). complete() doesn't know how to bridge this gap
        # (that's why we're here in the residue block). So we bridge this gap
        # manually, making sure to collect progress in *bundle*.
        row = {BytesMM::Completion.new, BytesMM.append(atom, APPEARANCE_SET_GAP)}

        lock.synchronize { bundle << row }
      end

      # At this point we know all completion fibers have terminated. We can use
      # bundle without a lock safely.

      bundle
    end

    # Yields appearances that are perceived by all of the given sensor *strands*
    # simultaneously.
    def each_appearance(atoms : IAtomsPresent, secret : Term?, strands : StrandList, & : WWID ->) : Nil
      secret_slice = Meridium.secret_slice(secret)

      wg = WaitGroup.new(strands.size)

      # Convert strands to bundles concurrently.
      bundles = [] of Array(BytesMM::Row)
      lock = Mutex.new

      strands.each do |strand|
        spawn do
          bundle = bundleof(atoms, secret_slice, strand)

          lock.synchronize { bundles << bundle }
        ensure
          wg.done
        end
      end

      wg.wait

      hasher = Atom::Hasher.new

      # To read a WWID, we need BYTESIZE bytes. Each byte consists of 4 base-4 digits.
      # We need to iterate one base-4 digit more so that we see there's nothing past
      # the BYTESIZE bytes we've read.
      (WWID::BYTESIZE*4 + 1).times do |ord|
        return if bundles.empty?

        expanded = BytesMM.expand2d(hasher, bundles)
        marked = BytesMM.mark2d(atoms, expanded)

        unless ord == WWID::BYTESIZE*4
          bundles = BytesMM.collapse2d(marked)

          # Index for cheap intersection
          populations = bundles.map do |bundle|
            bundle.to_set { |completion, _| completion }
          end

          # Select only those completions that are present in all other bundles.
          bundles.each do |bundle0|
            xsect = bundle0.select! do |completion, _|
              populations.all? { |population| completion.in?(population) }
            end

            # If any bundle ends up being empty, then all other bundles will
            # be empty and so on. No point in continuing to complete.
            return if xsect.empty?
          end

          next
        end

        marked.each do |bundle|
          # Dead-end completions at this point are valid completions. Process them.
          BytesMM.collapse(bundle) do |completion|
            bytesize = completion.bytesize
            unless bytesize == WWID::BYTESIZE
              Log.debug { "reject entry: unexpected entry bytesize #{bytesize}" }
              next
            end

            scratch = uninitialized UInt8[WWID::BYTESIZE]
            entry = scratch.to_slice
            completion.final_to(entry, key: Bytes.empty, prefix: Bytes.empty)

            begin
              appearance = WWID.from_slice_be(entry)
            rescue e : WWID::ParseError
              Log.debug(exception: e) { "reject entry" }
              next
            end

            yield appearance
          end
        end

        break
      end
    end
  end
end
