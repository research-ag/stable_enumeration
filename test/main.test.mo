// @testmode wasi
import { test; suite } "mo:test";
import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Nat8 "mo:base/Nat8";
import Principal "mo:base/Principal";
import Enumeration "../src/lib";

class RNG() {
  var seed = 234234;

  public func next() : Nat {
    seed += 1;
    let a = seed * 15485863;
    a * a * a % 2038074743;
  };

  public func blob() : Blob {
    let a = Array.tabulate<Nat8>(29, func(i) = Nat8.fromNat(next() % 256));
    Blob.fromArray(a);
  };

  public func maxBlob() : Blob {
    let a = Array.tabulate<Nat8>(29, func(i) = Nat8.fromNat(0));
    Blob.fromArray(a);
  };
};

suite(
  "Enumeration",
  func() {
    test(
      "Blob",
      func() {
        let n = 100;
        let r = RNG();
        let b = Enumeration.Enumeration();
        let blobs = Array.tabulate<Blob>(n, func(i) = r.blob());

        var i = 0;

        assert (b.size() == 0);
        i := 0;
        while (i < n) {
          assert (b.add(blobs[i]) == i);
          assert (b.size() == i + 1);
          i += 1;
        };

        i := 0;
        while (i < n) {
          assert (b.add(blobs[i]) == i);
          assert (b.size() == n);
          i += 1;
        };

        b.unsafeUnshare(b.share());

        i := 0;
        while (i < n) {
          assert (b.lookup(blobs[i]) == ?i);
          i += 1;
        };

        i := 0;
        while (i < n) {
          assert (b.lookup(r.blob()) == null);
          i += 1;
        };

        i := 0;
        while (i < n) {
          assert (b.get(i) == blobs[i]);
          i += 1;
        };
      },
    );
    test(
      "Empty blob",
      func() {
        let b = Enumeration.Enumeration();
        let blob = Principal.toBlob(Principal.fromText("aaaaa-aa"));
        assert b.add(blob) == 0;
        assert b.get(0) == blob;
        assert b.lookup(blob) == ?0;
      },
    );
  },
);
