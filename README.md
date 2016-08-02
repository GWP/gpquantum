# gpquantum

This is an evolution system on top of a quantum computer simulator, written in Clojure. The simulator was translated from a Clojurescript simulator written by Omri Bernstein, which was translated from a Common Lisp simulator written by Lee Spector.

## Overview

The core.clj file contains everything related to the evolution. All other source files are part of the simulator. The entry point to the simulator is test-quantum-program, with syntax as follows:
    
    (test-quantum-program :program program :read-from [0] :cases cases)
    
program: A list of quantum gates, you can generate random ones using generate-qgame-program
read-from: The output qubit, the default used is 0.
cases: Defines what problem you are testing. Examples for the OR-problem, and the AND-OR-problem can be found in core.clj

## Setup



## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
