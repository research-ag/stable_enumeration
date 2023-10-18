/// `Enumeration<K>` is a "set enumeration" of elements of type `K` called "keys".
///
/// A typical application is to assign permanent user numbers to princpals.
///
/// The data structure is a map `Nat -> K` with the following properties:
/// * keys are not repeated, i.e. the map is injective
/// * keys are consecutively numbered (no gaps), i.e. if n keys are stored
///   then `[0,n) -> K` is bijective
/// * keys are numbered in the order they are added to the data structure
/// * keys cannot be deleted
/// * efficient inverse lookup `K -> Nat`
/// * doubles as a set implementation (without deletion)
///
/// The data structure is optimized primarily for memory efficiency
/// and secondarily for instruction efficiency.
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

module {
  public type BufferRep = {
    bytes : Region;
    var bytes_count : Nat64;

    elems : Region;
    var elems_count : Nat64;
  };

  let elem_size = 16 : Nat64; /* two Nat64s, for pos and size. */

  func regionEnsureSizeBytes(r : Region, new_byte_count : Nat64) {
    let pages = Region.size(r);
    if (new_byte_count > pages << 16) {
      let new_pages = ((new_byte_count + ((1 << 16) - 1)) / (1 << 16)) - pages;
      assert Region.grow(r, new_pages) == pages;
    };
  };

  func bufferUnwrap(rep : ?BufferRep) : BufferRep {
    let ?self = rep else Prim.trap("Buffer rep is uninitialized");
    self;
  };

  func bufferAdd(rep : ?BufferRep, blob : Blob) {
    let self = bufferUnwrap(rep);
    let elem_i = self.elems_count;
    self.elems_count += 1;

    let elem_pos = self.bytes_count;
    self.bytes_count += Prim.natToNat64(blob.size());

    regionEnsureSizeBytes(self.bytes, self.bytes_count);
    Region.storeBlob(self.bytes, elem_pos, blob);

    regionEnsureSizeBytes(self.elems, self.elems_count * elem_size);
    Region.storeNat64(self.elems, elem_i * elem_size + 0, elem_pos);
    Region.storeNat64(self.elems, elem_i * elem_size + 8, Prim.natToNat64(blob.size()));
  };

  func bufferGet(rep : ?BufferRep, index : Nat64) : Blob {
    let self = bufferUnwrap(rep);

    assert index < self.elems_count;
    let pos = Region.loadNat64(self.elems, index * elem_size);
    let size = Region.loadNat64(self.elems, index * elem_size + 8);
    Region.loadBlob(self.bytes, pos, Prim.nat64ToNat(size));
  };

  func bufferSize(rep : ?BufferRep) : Nat {
    let self = bufferUnwrap(rep);
    Nat64.toNat(self.elems_count);
  };

  /// Red-black tree of key `Nat`.
  public type Tree = ?({ #R; #B }, Tree, Nat, Tree);

  /// Common functions between both classes
  func lbalance(left : Tree, y : Nat, right : Tree) : Tree {
    switch (left, right) {
      case (?(#R, ?(#R, l1, y1, r1), y2, r2), r) ?(#R, ?(#B, l1, y1, r1), y2, ?(#B, r2, y, r));
      case (?(#R, l1, y1, ?(#R, l2, y2, r2)), r) ?(#R, ?(#B, l1, y1, l2), y2, ?(#B, r2, y, r));
      case _ ?(#B, left, y, right);
    };
  };

  func rbalance(left : Tree, y : Nat, right : Tree) : Tree {
    switch (left, right) {
      case (l, ?(#R, l1, y1, ?(#R, l2, y2, r2))) ?(#R, ?(#B, l, y, l1), y1, ?(#B, l2, y2, r2));
      case (l, ?(#R, ?(#R, l1, y1, r1), y2, r2)) ?(#R, ?(#B, l, y, l1), y1, ?(#B, r1, y2, r2));
      case _ ?(#B, left, y, right);
    };
  };

  // approximate growth by sqrt(2) by 2-powers
  // the function will trap if n == 0 or n >= 3 * 2 ** 30
  func next_size(n_ : Nat) : Nat {
    if (n_ == 1) return 2;
    let n = Nat32.fromNat(n_); // traps if n >= 2 ** 32
    let s = 30 - Nat32.bitcountLeadingZero(n); // traps if n == 0
    let m = ((n >> s) +% 1) << s;
    assert (m != 0); // traps if n >= 3 * 2 ** 30
    Nat32.toNat(m);
  };

  /// An optimized version of Enumeration<Blob>
  public class Enumeration() {
    private var stableRep : ?BufferRep = null;

    private var tree = (null : Tree);

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
      let size = bufferSize(stableRep);
      var index = size;

      func insert(tree : Tree) : Tree {
        switch tree {
          case (?(#B, left, y, right)) {
            let res = Prim.blobCompare(key, bufferGet(stableRep, Nat64.fromNat(y)));
            if (res < 0) {
              lbalance(insert(left), y, right);
            } else if (res > 0) {
              rbalance(left, y, insert(right));
            } else {
              index := y;
              tree;
            };
          };
          case (?(#R, left, y, right)) {
            let res = Prim.blobCompare(key, bufferGet(stableRep, Nat64.fromNat(y)));
            if (res < 0) {
              ?(#R, insert(left), y, right);
            } else if (res > 0) {
              ?(#R, left, y, insert(right));
            } else {
              index := y;
              tree;
            };
          };
          case (null) {
            index := size;
            ?(#R, null, size, null);
          };
        };
      };

      tree := switch (insert(tree)) {
        case (?(#R, left, y, right)) ?(#B, left, y, right);
        case other other;
      };

      if (index == size) {
        bufferAdd(stableRep, key);
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
      func get_in_tree(x : Blob, t : Tree) : ?Nat {
        switch t {
          case (?(_, l, y, r)) {

            let res = Prim.blobCompare(key, bufferGet(stableRep, Nat64.fromNat(y)));
            if (res < 0) {
              get_in_tree(x, l);
            } else if (res > 0) {
              get_in_tree(x, r);
            } else {
              ?y;
            };
          };
          case (null) null;
        };
      };

      get_in_tree(key, tree);
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
      bufferGet(stableRep, Nat64.fromNat(index));
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
    public func size() : Nat = bufferSize(stableRep);

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
    public func share() : (Tree, BufferRep) {
      (tree, bufferUnwrap(stableRep));
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
    public func unsafeUnshare(data : (Tree, BufferRep)) {
      tree := data.0;
      stableRep := ?data.1;
    };
  };
};
