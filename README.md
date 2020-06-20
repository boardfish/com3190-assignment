Erl-nigma
=====

A simulation of the Enigma I in Erlang.

Build
-----

    $ rebar3 escriptize
    
Run
---

    $ ./_build/default/bin/enigma "Text you want to encrypt here"

Command line arguments aren't implemented, so you'll want to change the arguments passed to the `main` method.

Supply:

- a reflector name (`"B"`)
- a triple of rotors (`"II", "I", "III"`)
- a triple of ring settings (`{26, 23, 4}` = Z, W, D Ringstellungen)
- a list of plugboard pairs (`[{$E, $Z}...]`)
- an initial setting (`{$A, $G, $I}`)
