# bit-marker
Software in OCaml for the circuit transformations is
../src

85_parser  - it is just a subproject that parsing .bench format
of itc circuit benchmark suit. later it is inclucded in the
whole project.

EPA_16 it's is Error Propagation analysis with 16 value logic.
M1 it's the project with 01TB encoding
M2 the project with precise encoding 01top0top1.

mvs_cpp it is project to calculate minimal vertex feedback set
- i took it from internet.

project M1-3 tries to merge three logic (01TB, precise, 1-value)
but it does not include output patters.

erra project is the cleanest and the lastest version.

In all files EPA is Error-Propagation analysis that includes
output patterns.
EAA - is an error-accumulation analysis that is just checking if an error
stays in the circuit or not.

All other projects can be ignored.

Each subfolder contains Build section. where we see not only executables
but also its betchmark and minimum-vertex feedback set executables (MVS-calculated).

In subfolder scripts- there are make and run scripts.
The run script also runs compiled executables.
