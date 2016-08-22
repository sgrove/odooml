# Dev setup

## Installation

Make sure you've already installed [OPAM](https://opam.ocaml.org/):

    brew install opam

Now run the following to checkout the project, create an opam switch
(like a sandbox so the dependencies in multiple projects don't cause
havoc with one another), and install all the dependencies (this will
take awhile if it's your first time, a lot to compile):

    git clone git@github.com:sgrove/odoomml.git
    make deps
    eval `opam config env`

To add opam dependencies, edit `Makefile.user` under the `deps` target
to add the opam invocation (you'll want to also update `.merlin` while
you're at it).

## Running

    make clean && make tri3 && ./trigl3.byte
    make clean && make tri4 && ./trigl4.byte
