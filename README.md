# Stable enumeration

Version of [enumeration](https://github.com/research-ag/enumeration) that stores information in regions.

`Enumeration` is a "set enumeration" of elements of `Blob`s called "keys".

A typical application is to assign permanent user numbers to princpals.

The data structure is a map `Nat -> Blob` with the following properties:
* keys are not repeated, i.e. the map is injective
* keys are consecutively numbered (no gaps), i.e. if n keys are stored
  then `[0,n) -> Blob` is bijective
* keys are numbered in the order they are added to the data structure
* keys cannot be deleted
* efficient inverse lookup `Blob -> Nat`
* doubles as a set implementation (without deletion)

## Copyright

MR Research AG, 2023

## Authors

Main author: Andrii Stepanov (AStepanov25).

Contributors: Timo Hanke (timohanke), Andrii Stepanov (AStepanov25).

## License 

Apache-2.0
