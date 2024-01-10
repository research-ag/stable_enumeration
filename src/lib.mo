/// `Enumeration` is a "set enumeration" of elements of `Blob`s called "keys".

/// A typical application is to assign permanent user numbers to princpals.

/// The data structure is a map `Nat -> Blob` with the following properties:
/// * keys are not repeated, i.e. the map is injective
/// * keys are consecutively numbered (no gaps), i.e. if n keys are stored
///   then `[0,n) -> Blob` is bijective
/// * keys are numbered in the order they are added to the data structure
/// * keys cannot be deleted
/// * efficient inverse lookup `Blob -> Nat`
/// * doubles as a set implementation (without deletion)
///
/// Copyright: 2023 MR Research AG
/// Main author: Andrii Stepanov (AStepanov25)
/// Contributors: Timo Hanke (timohanke), Yurii Pytomets (Pitometsu)

import Blob "mo:base/Blob";
import Array "mo:base/Array";
import Nat32 "mo:base/Nat32";
import Prim "mo:⛔";
import Region "mo:base/Region";
import Nat64 "mo:base/Nat64";
import BTree "mo:btree/BTree";

module {
  public type BufferRep = {
    bytes : Region;
    var bytes_count : Nat64;

    elems : Region;
    var elems_count : Nat64;
  };

  module Buffer {
    let elem_size = 16 : Nat64; /* two Nat64s, for pos and size. */

    func regionEnsureSizeBytes(r : Region, new_byte_count : Nat64) {
      let pages = Region.size(r);
      if (new_byte_count > pages << 16) {
        let new_pages = ((new_byte_count + ((1 << 16) - 1)) / (1 << 16)) - pages;
        assert Region.grow(r, new_pages) == pages;
      };
    };

    public func new() : BufferRep = {
      bytes = Region.new();
      var bytes_count = 0;
      elems = Region.new();
      var elems_count = 0;
    };

    public func add(self : BufferRep, blob : Blob) {
      let elem_i = self.elems_count;
      self.elems_count += 1;

      let elem_pos = self.bytes_count;
      self.bytes_count += Prim.natToNat64(blob.size());

      regionEnsureSizeBytes(self.bytes, self.bytes_count);
      if (blob.size() != 0) {
        Region.storeBlob(self.bytes, elem_pos, blob);
      };

      regionEnsureSizeBytes(self.elems, self.elems_count * elem_size);
      Region.storeNat64(self.elems, elem_i * elem_size + 0, elem_pos);
      Region.storeNat64(self.elems, elem_i * elem_size + 8, Prim.natToNat64(blob.size()));
    };

    public func get(self : BufferRep, index : Nat64) : Blob {
      assert index < self.elems_count;
      let pos = Region.loadNat64(self.elems, index * elem_size);
      let size = Region.loadNat64(self.elems, index * elem_size + 8);
      if (size != 0) {
        Region.loadBlob(self.bytes, pos, Prim.nat64ToNat(size));
      } else {
        Blob.fromArray([]);
      };
    };

    public func size(self : BufferRep) : Nat {
      Nat64.toNat(self.elems_count);
    };
  };

  public type StableData = {
    btree : BTree.BTree<Blob, Nat>;
    array : BufferRep;
  };

  /// An optimized version of Enumeration<Blob>
  public class Enumeration() {
    let value_conv = BTree.nconv(8);
    let key_conv = BTree.noconv(29);
    private var _state : ?StableData = null;

    func state() : StableData {
      switch (_state) {
        case (?s) s;
        case (null) {
          let ret = {
            btree = BTree.new<Blob, Nat>(key_conv, value_conv);
            array = Buffer.new();
          };
          _state := ?ret;
          ret;
        };
      };
    };

    /// Add `key` to enumeration. Returns `size` if the key in new to the enumeration and index of key in enumeration otherwise.
    ///
    /// Example:
    /// ```motoko
    /// let e = Enumeration.EnumerationBlob();
    /// assert(e.add("abc") == 0);
    /// assert(e.add("aaa") == 1);
    /// assert(e.add("abc") == 0);
    /// ```
    /// Runtime: O(log(n))
    public func add(key : Blob) : Nat {
      let size = Buffer.size(state().array);
      let index = BTree.getOrPut(state().btree, key_conv, key, value_conv, size);
      if (index == size) {
        Buffer.add(state().array, key);
      };
      index;
    };

    /// Returns `?index` where `index` is the index of `key` in order it was added to enumeration, or `null` it `key` wasn't added.
    ///
    /// Example:
    /// ```motoko
    /// let e = Enumeration.EnumerationBlob();
    /// assert(e.add("abc") == 0);
    /// assert(e.add("aaa") == 1);
    /// assert(e.lookup("abc") == ?0);
    /// assert(e.lookup("aaa") == ?1);
    /// assert(e.lookup("bbb") == null);
    /// ```
    /// Runtime: O(log(n))
    public func lookup(key : Blob) : ?Nat {
      BTree.get(state().btree, key_conv, key, value_conv);
    };

    /// Returns `K` with index `index`. Traps it index is out of bounds.
    ///
    /// Example:
    /// ```motoko
    /// let e = Enumeration.EnumerationBlob();
    /// assert(e.add("abc") == 0);
    /// assert(e.add("aaa") == 1);
    /// assert(e.get(0) == "abc");
    /// assert(e.get(1) == "aaa");
    /// ```
    /// Runtime: O(1)
    public func get(index : Nat) : Blob {
      Buffer.get(state().array, Nat64.fromNat(index));
    };

    /// Returns number of unique keys added to enumeration.
    ///
    /// Example:
    /// ```motoko
    /// let e = Enumeration.EnumerationBlob();
    /// assert(e.add("abc") == 0);
    /// assert(e.add("aaa") == 1);
    /// assert(e.size() == 2);
    /// ```
    /// Runtime: O(1)
    public func size() : Nat = Buffer.size(state().array);

    /// Returns pair of red-black tree for map from `K` to `Nat` and
    /// array of `K` for map from `Nat` to `K`.
    ///
    /// Example:
    /// ```motoko
    /// let e = Enumeration.EnumerationBlob();
    /// assert(e.add("abc") == 0);
    /// assert(e.add("aaa") == 1);
    /// e.unsafeUnshare(e.share()); // Nothing changed
    /// ```
    /// Runtime: O(1)
    public func share() : StableData {
      state();
    };

    /// Sets internal content from red-black tree for map from `K` to `Nat`
    /// and array of `K` for map from `Nat` to `K`.
    /// `t` should be a valid red-black tree and correspond to array `a`.
    /// This function does not perform any validation.
    ///
    /// Example:
    /// ```motoko
    /// let e = Enumeration.EnumerationBlob();
    /// assert(e.add("abc") == 0);
    /// assert(e.add("aaa") == 1);
    /// e.unsafeUnshare(e.share()); // Nothing changed
    /// ```
    /// Runtime: O(1)
    public func unsafeUnshare(data : StableData) {
      _state := ?data;
    };
  };
};
