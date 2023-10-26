import Enumeration "../../src/lib";
import Region "mo:base/Region";

actor {
  public query func greet(name : Text) : async Text {
    let e = Enumeration.Enumeration();

    assert (e.add("abc") == 0);
    assert (e.add("aaa") == 1);
    assert (e.add("abc") == 0);
    
    assert(e.lookup("abc") == ?0);
    assert(e.lookup("aaa") == ?1);
    assert(e.lookup("bbb") == null);

    return "Hello, " # name # "!";
  };
};
