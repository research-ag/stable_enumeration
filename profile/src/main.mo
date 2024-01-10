import Enumeration "../../src/lib";
import Region "mo:base/Region";
import Debug "mo:base/Debug";
import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Nat8 "mo:base/Nat8";
import Option "mo:base/Option";
import Nat64 "mo:base/Nat64";
import E "mo:base/ExperimentalInternetComputer";
import Nat "mo:base/Nat";
import Int "mo:base/Int";
import Float "mo:base/Float";
import { test; suite } "mo:test";

actor {
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

  public query func profile() : async () {
    let n = 2 ** 13;
    let r = RNG();
    let enum = Enumeration.Enumeration();
    let blobs = Array.tabulate<Blob>(n, func(i) = r.blob());

    var m = 1;
    ignore enum.add(blobs[0]);

    while (m < n) {
      let blob = r.blob();
      Debug.print("n = " # Nat.toText(m) # " instructions = " # Nat.toText(Nat64.toNat(E.countInstructions(func() = ignore enum.lookup(blob)))) # " memory tree = " # Nat64.toText(Region.size(enum.share().btree.region)) # " memory total = " # Nat64.toText(Region.size(enum.share().array.bytes) + Region.size(enum.share().array.elems)));

      var i = m;
      while (i < 2 * m) {
        ignore enum.add(blobs[i]);
        i += 1;
      };
      m *= 2;
    };
  };

  public query func profile_size(n : Nat) : async Float {
    let r = RNG();
    let enum = Enumeration.Enumeration();
    let blobs = Array.tabulate<Blob>(n, func(i) = r.blob());

    ignore enum.add(blobs[0]);
    let one = Nat64.toNat(E.countInstructions(func() = ignore enum.lookup(blobs[0])));

    var i = 1;
    while (i < n) {
      ignore enum.add(blobs[i]);
      i += 1;
    };

    i := 0;
    var sum = 0;
    while (i < Nat.min(n, 10)) {
      sum += Nat64.toNat(E.countInstructions(func() = ignore enum.lookup(blobs[i])));
      i += 1;
    };
    Float.fromInt(sum) / Float.fromInt(Nat.min(n, 10)) / Float.fromInt(one) / Float.log(Float.fromInt(n));
  };

  let enum = Enumeration.Enumeration();
  let r = RNG();

  public shared func add() : async () {
    let n = 2 ** 12;
    var i = 0;
    while (i < n) {
      ignore enum.add(r.blob());
      i += 1;
    };
  };

  public shared func add_many() : async () {
    await add();
    while (true) {
      let n = enum.size() / 2 ** 12;
      var i = 0;
      while (i < n) {
        await add();
        i += 1;
      };
      let blob = r.blob();
      Debug.print(debug_show (E.countInstructions(func() = ignore enum.lookup(blob)), enum.size()));
    };
  };
};
