import Enumeration "../../src/lib";
import Region "mo:base/Region";

actor {
  public query func greet(name : Text) : async Text {
    let e = Enumeration.Enumeration();
    e.unsafeUnshare(
      null,
      {
        bytes = Region.new();
        var bytes_count = 0;
        elems = Region.new();
        var elems_count = 0;
      },
    );

    assert (e.add("abc") == 0);
    assert (e.add("aaa") == 1);
    assert (e.add("abc") == 0);
    
    assert(e.lookup("abc") == ?0);
    assert(e.lookup("aaa") == ?1);
    assert(e.lookup("bbb") == null);

    return "Hello, " # name # "!";
  };
};
